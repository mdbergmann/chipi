import { defineConfig } from 'vite';

export default defineConfig({
  appType: 'spa',
  server: {
    port: 5173,
    proxy: {
      '^/(items|itemgroups).*': {  // proxy both /items… and /itemgroups…
        target: 'http://localhost:8765',
        changeOrigin: true
      }
    }
  }
});
