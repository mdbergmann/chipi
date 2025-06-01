import { defineConfig } from 'vite';

export default defineConfig({
  appType: 'spa',
  server: {
    port: 5173,
    proxy: {
      '^/items.*': {               // GET /items, POST /items/…
        target: 'http://localhost:8765',
        changeOrigin: true
      }
    }
  }
});
