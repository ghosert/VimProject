// Update connection status
function updateOnlineStatus() {
    const status = document.getElementById('connection-status');
    status.textContent = navigator.onLine ? "Online" : "Offline";
    status.style.color = navigator.onLine ? "green" : "red";
}
window.addEventListener('online', updateOnlineStatus);
window.addEventListener('offline', updateOnlineStatus);
updateOnlineStatus();

// ==========================================
// Service Worker Registration Logic
// ==========================================

if ('serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        // Service Workers generally require HTTPS or localhost.
        // This check helps debug issues when testing on IP addresses.
        if (window.location.protocol === 'http:' && 
            window.location.hostname !== 'localhost' && 
            window.location.hostname !== '127.0.0.1') {
            const statusBox = document.querySelector('.status-box');
            
            // Create a container for the warnings
            const warningContainer = document.createElement('div');
            warningContainer.className = 'warning-box';
            
            warningContainer.innerHTML = `
                <p style="color: red; font-weight: bold; margin-top: 0;">
                    ⚠️ Service Worker might fail! You are on an insecure origin (HTTP + IP). Please use <code>localhost</code> or HTTPS.
                </p>
                <hr style="border: 0; border-top: 1px solid #ffeeba; margin: 10px 0;">
                <strong>To make this work for testing on HTTP + IP:</strong>
                <ol style="margin: 5px 0 0 0; padding-left: 20px;">
                    <li>Open <code>chrome://flags/#unsafely-treat-insecure-origin-as-secure</code></li>
                    <li><strong>Enable</strong> the flag.</li>
                    <li>Add <code>${window.location.origin}</code> to the text box.</li>
                    <li>Relaunch Chrome.</li>
                </ol>
            `;
            
            statusBox.appendChild(warningContainer);
            console.warn('Service Workers require a secure context (HTTPS or localhost).');
        }

        // Register the Service Worker
        navigator.serviceWorker.register('/sw.js')
            .then(registration => {
                console.log('SW registered: ', registration);

                // Check if there's already a service worker waiting: if there are sw.js changes when user visited the page before, a new SW was installed, but never got refreshed.
                if (registration.waiting) {
                    showUpdateNotification(registration);
                    return;
                }

                // Listen for new service workers installing, if there are sw.js changes, this event fires.
                registration.onupdatefound = () => {
                    const newWorker = registration.installing;
                    
                    // after completing install, this onstatechange event fires to notify SW installed.
                    newWorker.onstatechange = () => {
                        if (newWorker.state === 'installed') {
                            if (navigator.serviceWorker.controller) { // a previous SW is already controlling the page (meaning this is an update, not a first-time install)
                                // New update available
                                console.log('New content is available; please refresh.');
                                showUpdateNotification(registration);
                            } else { // no previous SW exists, means this is first-time install, actually very first time to visit this page, also means content is cached for offline.
                                // Content is cached for the first time
                                console.log('Content is cached for offline use.');
                            }
                        }
                    };
                };
            })
            .catch(registrationError => {
                console.log('SW registration failed: ', registrationError);
            });
        // Scenarios:
        // 1. Very first time to visit the page, line 70 runs.
        // 2. Refresh the page without sw.js changes, start with line 49, but nothing else will run
        // 3. Refresh the page with sw.js changes, line 66 runs
        // 4. in step 3 above, if you don't click on update button, and refresh again, line 53 runs
    });

    // Handle controller change (when new SW takes over)
    let refreshing = false;
    navigator.serviceWorker.addEventListener('controllerchange', () => {
        if (!refreshing) {
            window.location.reload();
            refreshing = true;
        }
    });
}

// Show the "Update" notification
function showUpdateNotification(registration) {
    const notification = document.getElementById('update-notification');
    notification.classList.remove('hidden');

    const updateBtn = document.getElementById('update-btn');
    updateBtn.onclick = async () => {
        // Check for any newer version before activating.
        // This handles the case where v3 was deployed while the v2 notification was showing.
        try {
            await registration.update();
        } catch (e) {
            // Update check failed (e.g., offline), proceed with current waiting SW
        }

        // If a new SW is installing, wait for it to finish
        if (registration.installing) {
            await new Promise(resolve => {
                registration.installing.addEventListener('statechange', function handler(e) {
                    if (e.target.state === 'installed' || e.target.state === 'redundant') {
                        e.target.removeEventListener('statechange', handler);
                        resolve();
                    }
                });
            });
        }

        // Now activate the latest waiting SW
        if (registration.waiting) {
            registration.waiting.postMessage({ type: 'SKIP_WAITING' });
        }

        // Hide notification
        notification.classList.add('hidden');
    };
}
