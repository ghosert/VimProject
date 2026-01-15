# Service Worker Demo

This project demonstrates how to use a Service Worker to replace Application Cache (AppCache). It covers:
1.  **Offline Support:** Caching static assets.
2.  **Update Flow:** Detecting new versions and prompting the user to refresh.
3.  **Local Testing:** Easy start/stop server.

## Prerequisites

- Node.js installed.

## Getting Started

1.  **Install Dependencies:**
    ```bash
    npm install
    ```

2.  **Start the Server:**
    ```bash
    node server.js
    ```
    Access the app at `http://localhost:3000`.

## How to Test

### 1. Offline Mode
1.  Open `http://localhost:3000`.
2.  Open Chrome DevTools (F12) -> **Network** tab.
3.  Check the **Offline** box (or stop the node server with `Ctrl+C`).
4.  Refresh the page. It should still load from the Service Worker cache!

### 2. Updating the App (The "Remind User" Feature)
To simulate an update (like changing JS/CSS):

1.  **Modify the Content:**
    - Open `public/index.html` and change the version text: `<strong id="app-version">v1.0.1</strong>`.
    
2.  **Update the Service Worker:**
    - Open `public/sw.js`.
    - Change `CACHE_NAME` from `'my-app-v1'` to `'my-app-v2'`.
    - **Note:** Changing the SW file byte-for-byte is required for the browser to detect an update.

3.  **See the Result:**
    - Refresh the page `http://localhost:3000`.
    - You will see a notification banner: **"A new version is available!"**.
    - Click **"Refresh to Update"**. The page will reload, and the new version (v1.0.1) will be visible.

## Troubleshooting

### "Offline Mode" Not Working?
**Are you using an IP address (e.g., `http://192.168.1.5:3000`)?**

Service Workers **only** work on **Secure Contexts**:
1.  **HTTPS** (e.g., `https://example.com`)
2.  **Loopback** (e.g., `http://localhost:3000` or `http://127.0.0.1:3000`)

If you access the site via an IP address over HTTP, the browser **blocks** the Service Worker for security reasons.

**Fix:** Use `http://localhost:3000` on your machine.

If you *must* test on a mobile device via IP:
1.  **Chrome Port Forwarding:** Connect phone via USB, go to `chrome://inspect/#devices`, and setup port forwarding (Map `3000` to `localhost:3000`).
2.  **Chrome Flags:** On your mobile device chrome, go to `chrome://flags/#unsafely-treat-insecure-origin-as-secure`, enable it, and add your IP address.

## How it Works

### Architecture

`sw.js` runs inside the browser but in a **separate thread** from the main page:

```
┌──────────────────────────────────────────────────────────────────┐
│                          BROWSER                                 │
├─────────────────────────────┬────────────────────────────────────┤
│                             │                                    │
│   MAIN THREAD               │   SERVICE WORKER THREAD            │
│   (index.html, app.js)      │   (sw.js)                          │
│                             │                                    │
│   ┌───────────────────┐     │   ┌───────────────────┐            │
│   │ DOM Access        │     │   │ No DOM Access     │            │
│   │ window object     │     │   │ self object       │            │
│   │ User interactions │     │   │ Intercepts fetch  │            │
│   └───────────────────┘     │   │ Manages cache     │            │
│                             │   └───────────────────┘            │
│                             │                                    │
│         ◄───── postMessage / events ─────►                       │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

The service worker is essentially a **programmable network proxy** that lives in your browser - it intercepts all network requests and can serve cached responses for offline functionality.

### Service Worker Lifecycle

```
  ┌──────────┐      ┌──────────┐      ┌──────────┐      ┌──────────┐
  │ Download │ ───► │ Install  │ ───► │ Waiting  │ ───► │ Activate │
  └──────────┘      └──────────┘      └──────────┘      └──────────┘
                         │                  │                 │
                   Cache files         Wait for old      Delete old
                                       SW to close       caches
                                           │
                                  User clicks "Update"
                                  ───► skipWaiting()
```

1. **Registration** (`app.js`): Registers the service worker and listens for `onupdatefound` to show update notifications.
2. **Install** (`sw.js`): Caches files listed in `FILES_TO_CACHE`.
3. **Waiting**: New SW waits until all tabs using old SW are closed, OR user clicks "Update" → sends `SKIP_WAITING` message.
4. **Activate** (`sw.js`): Deletes old caches not matching current `CACHE_NAME`.

### Fetch Interception (Cache-First Strategy)


```
   Browser Request
        │
        ▼
┌───────────────────────────────────────────────────────────────────┐
│                     SERVICE WORKER                                │
│                                                                   │
│   ┌──────────────┐     YES    ┌────────────────────────────────┐  │
│   │ In Cache     │ ─────────► │ Return from Cache Storage      │  │
│   │ Storage?     │            │ (Works offline!)               │  │
│   └──────────────┘            └────────────────────────────────┘  │
│          │                                                        │
│          │ NO                                                     │
│          ▼                                                        │
│   ┌──────────────┐                                                │
│   │ fetch()      │ ─────────────────────────────┐                 │
│   └──────────────┘                              │                 │
│                                                 │                 │
└─────────────────────────────────────────────────┼─────────────────┘
                                                  │
                                                  ▼
                                    ┌──────────────────────────┐
                                    │       HTTP CACHE         │
                                    │   (Browser Cache)        │
                                    │                          │
                                    │  ┌──────────┐    YES     │
                                    │  │ Valid    │ ─────────► │ Return cached
                                    │  │ cached   │            │ response
                                    │  │ response?│            │
                                    │  └──────────┘            │
                                    │       │                  │
                                    │       │ NO / Expired     │
                                    │       ▼                  │
                                    └───────┼──────────────────┘
                                            │
                                            ▼
                                    ┌──────────────────┐
                                    │     SERVER       │
                                    │  (Origin)        │
                                    └──────────────────┘
```


### Update Flow

When a new service worker is installed, it enters a **waiting state** by default. The browser keeps the old SW active to avoid breaking open tabs. The new SW will only activate when:

1. **All tabs are closed** - User closes every tab using the old SW, then reopens the site
2. **OR `skipWaiting()` is called** - The new SW forces itself to take over immediately

This demo uses `skipWaiting()` for a better user experience:

```
┌─────────┐    detects new SW    ┌─────────┐   user clicks   ┌──────────┐
│ app.js  │ ──────────────────►  │ Show    │ ──────────────► │ Send     │
│         │                      │ "Update"│   "Update"      │ SKIP_    │
└─────────┘                      │ Button  │                 │ WAITING  │
                                 └─────────┘                 └──────────┘
                                                                   │
                                      ┌────────────────────────────┘
                                      ▼
                              ┌──────────────┐
                              │ New SW takes │ ───► Page reloads
                              │ control      │
                              └──────────────┘
```

The relevant code:

- **`app.js:98`** - Sends the message: `worker.postMessage({ type: 'SKIP_WAITING' })`
- **`sw.js:87-91`** - Receives and calls `self.skipWaiting()` to force activation

Without this mechanism, users would have to close all tabs and reopen the site to get the update.


### Multi-Tab Update Behavior

When user clicks "Update" in one tab, **all tabs** get updated automatically:

```
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│   Tab 1     │  │   Tab 2     │  │   Tab 3     │
│  (clicks    │  │  (open)     │  │  (open)     │
│   Update)   │  │             │  │             │
└──────┬──────┘  └──────┬──────┘  └──────┬──────┘
       │                │                │
       ▼                │                │
  postMessage           │                │
  SKIP_WAITING          │                │
       │                │                │
       ▼                ▼                ▼
┌─────────────────────────────────────────────────┐
│           SERVICE WORKER (new)                  │
│                                                 │
│  skipWaiting() ──► activates immediately        │
│                                                 │
│  Fires 'controllerchange' to ALL clients        │
└─────────────────────────────────────────────────┘
       │                │                │
       ▼                ▼                ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│   Tab 1     │  │   Tab 2     │  │   Tab 3     │
│  RELOADS    │  │  RELOADS    │  │  RELOADS    │
└─────────────┘  └─────────────┘  └─────────────┘
```

Every tab listens for the `controllerchange` event (`app.js:81-87`). When the new SW takes control, the browser fires this event to **all tabs**, and each tab reloads itself automatically.

### Server Configuration

`server.js` serves `sw.js` with `Cache-Control: no-cache`. This is **crucial** for development so the browser checks for updates on every page load. In production, you might set a short max-age (e.g., 0 or 1 hour), but `no-cache` is safest to avoid "stale" workers.

## Emergency Recovery (Cached Bug)

If you deploy a Service Worker that caches a broken version of your app (e.g., a JS bug that crashes the page), you need to "break" the cache.

**Solution:**
1.  Fix the bug in your JS/HTML.
2.  Update `public/sw.js`:
    - Increment `CACHE_NAME` (e.g., `v3`).
    - This ensures the `activate` event fires and deletes the old, buggy cache (`v2`).
3.  Deploy.
    - Users will still load the broken version *one last time* from the cache.
    - The browser will detect the new `sw.js` in the background.
    - The new SW will install and wait.
    - Depending on your logic, you might need to force reload or wait for the user to close/reopen the tab.

**Nuclear Option (Kill Switch):**
If the SW logic itself is broken (e.g., the `fetch` handler is buggy), deploy a new `sw.js` that unregisters itself:

```javascript
self.addEventListener('install', () => {
    self.skipWaiting();
});

self.addEventListener('activate', () => {
    self.registration.unregister()
        .then(() => self.clients.matchAll())
        .then(clients => {
            clients.forEach(client => client.navigate(client.url)); // Force reload
        });
});
```
