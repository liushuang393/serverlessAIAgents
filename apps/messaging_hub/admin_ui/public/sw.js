const CACHE_NAME = "messaging-hub-shell-v2";
const APP_SHELL = [
  "/",
  "/index.html",
  "/manifest.webmanifest",
  "/favicon.svg",
  "/icons/icon-192.png",
  "/icons/icon-512.png",
];
const DEV_PATH_PREFIXES = ["/@vite", "/@react-refresh", "/src/", "/node_modules/"];

function isDevOnlyRequest(requestUrl) {
  return DEV_PATH_PREFIXES.some((prefix) => requestUrl.pathname.startsWith(prefix));
}

function isStaticAsset(pathname) {
  return /\.(?:js|css|png|jpe?g|gif|svg|ico|webp|woff2?)$/i.test(pathname);
}

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches
      .open(CACHE_NAME)
      .then((cache) => cache.addAll(APP_SHELL))
      .then(() => self.skipWaiting()),
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((keys) =>
        Promise.all(
          keys
            .filter((key) => key !== CACHE_NAME)
            .map((key) => caches.delete(key)),
        ),
      )
      .then(() => self.clients.claim()),
  );
});

self.addEventListener("fetch", (event) => {
  const { request } = event;
  if (request.method !== "GET") {
    return;
  }

  const requestUrl = new URL(request.url);
  if (requestUrl.origin !== self.location.origin) {
    return;
  }
  if (
    requestUrl.pathname.startsWith("/api") ||
    requestUrl.pathname.startsWith("/ws") ||
    isDevOnlyRequest(requestUrl) ||
    requestUrl.searchParams.has("t") ||
    requestUrl.searchParams.has("import")
  ) {
    return;
  }

  if (request.mode === "navigate") {
    event.respondWith(fetch(request).catch(() => caches.match("/index.html")));
    return;
  }

  if (!isStaticAsset(requestUrl.pathname)) {
    event.respondWith(
      fetch(request).catch(() => caches.match(request)),
    );
    return;
  }

  event.respondWith(
    caches.match(request).then((cachedResponse) => {
      if (cachedResponse) {
        return cachedResponse;
      }
      return fetch(request).then((networkResponse) => {
        if (networkResponse.ok && networkResponse.type === "basic") {
          const cloned = networkResponse.clone();
          void caches
            .open(CACHE_NAME)
            .then((cache) => cache.put(request, cloned));
        }
        return networkResponse;
      });
    }),
  );
});
