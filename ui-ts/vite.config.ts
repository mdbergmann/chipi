import { defineConfig, loadEnv } from 'vite';

export default defineConfig(({ mode }) => {
  /* load .env[.local] and VITE_… variables                */
  const env = loadEnv(mode, process.cwd(), '');
  const host = env.VITE_API_HOST || '127.0.0.1';
  const port = env.VITE_API_PORT || '8765';

  return {
    appType: 'spa',
    server: {
      port: 5173,
      proxy: {
        '^/(items|itemgroups).*': {
          target: `http://${host}:${port}`,
          changeOrigin: true
        }
      }
    }
  };
});
