// ==========================================
// Service Worker Implementation
// ==========================================

// 1. CACHE_NAME: Change this value to trigger an update.
//    When you change this (e.g., 'my-app-v2'), the 'activate' event will fire,
//    deleting the old cache ('my-app-v1').
const CACHE_NAME = 'my-app-v1';

// 2. FILES_TO_CACHE: List of files to cache for offline use.
//    If any of these fail to fetch, the install step will fail.
const FILES_TO_CACHE = [
    '/',
    '/index.html',
    '/style.css',
    '/app.js'
];

// ------------------------------------------
// INSTALL EVENT
// ------------------------------------------
// Triggered when the browser sees a new SW file (byte-different)
self.addEventListener('install', event => {
    console.log('[ServiceWorker] Install');
    
    // Perform install steps: open cache and add files
    event.waitUntil(
        caches.open(CACHE_NAME)
            .then(cache => {
                console.log('[ServiceWorker] Caching app shell');
                return cache.addAll(FILES_TO_CACHE);
            })
    );
});

// ------------------------------------------
// ACTIVATE EVENT
// ------------------------------------------
// Triggered when the new SW is ready to take over.
// Good place to clean up old caches.
self.addEventListener('activate', event => {
    console.log('[ServiceWorker] Activate');

    event.waitUntil(
        caches.keys().then(keyList => {
            return Promise.all(keyList.map(key => {
                // Remove caches that don't match the current CACHE_NAME
                if (key !== CACHE_NAME) {
                    console.log('[ServiceWorker] Removing old cache', key);
                    return caches.delete(key);
                }
            }));
        })
    );

    // EMERGENCY RECOVERY TIP:
    // If you deployed a buggy Service Worker that breaks clients, 
    // you can deploy a new SW that immediately unregisters itself:
    // self.registration.unregister().then(() => self.clients.matchAll())
    //     .then(clients => clients.forEach(client => client.navigate(client.url)));
});

// ------------------------------------------
// FETCH EVENT
// ------------------------------------------
// Intercept network requests.
self.addEventListener('fetch', event => {
    // console.log('[ServiceWorker] Fetch', event.request.url);

    // Strategy: Cache First, falling back to Network
    event.respondWith(
        caches.match(event.request)
            .then(response => {
                // Cache hit - return response
                if (response) {
                    return response;
                }
                return fetch(event.request);
            })
    );
});

// ------------------------------------------
// MESSAGE EVENT (For "Skip Waiting")
// ------------------------------------------
// Listen for messages from the client (app.js)
self.addEventListener('message', event => {
    if (event.data && event.data.type === 'SKIP_WAITING') {
        // Force this SW to become the active service worker, ACTIVATE EVENT fires above, in this case, to clean up old caches
        self.skipWaiting();
    }
});
