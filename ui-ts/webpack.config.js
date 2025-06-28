const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './src/main.ts',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist'),
    clean: true,
  },
  resolve: {
    extensions: ['.ts', '.js'],
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
      {
        test: /\.css$/i,
        use: ['style-loader', 'css-loader'],
      },
    ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './src/index.html',
    }),
  ],
  devServer: {
    static: './dist',
    port: 3000,
    hot: true,
    headers: {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, PATCH, OPTIONS',
      'Access-Control-Allow-Headers': 'X-Requested-With, content-type, Authorization, X-Api-Key',
    },
    proxy: [
      {
        context: ['/items', '/itemgroups'],
        target: 'http://localhost:8765',
        changeOrigin: true,
        secure: false,
        logLevel: 'debug',
        onProxyReq: (proxyReq, req, res) => {
          console.log('Proxying request:', req.method, req.url);
        },
        onError: (err, req, res) => {
          console.error('Proxy error:', err);
        },
      },
      {
        context: ['/events'],
        target: 'http://localhost:8765',
        changeOrigin: true,
        secure: false,
        logLevel: 'debug',
        // Spezielle Konfiguration für SSE
        onProxyReq: (proxyReq, req, res) => {
          console.log('Proxying SSE request:', req.method, req.url);
          // SSE-spezifische Headers
          proxyReq.setHeader('Accept', 'text/event-stream');
          proxyReq.setHeader('Cache-Control', 'no-cache');
        },
        onProxyRes: (proxyRes, req, res) => {
          console.log('SSE Response headers:', proxyRes.headers);
          // SSE-Response-Headers weiterleiten
          if (proxyRes.headers['content-type']?.includes('text/event-stream')) {
            res.setHeader('Content-Type', 'text/event-stream');
            res.setHeader('Cache-Control', 'no-cache');
            res.setHeader('Connection', 'keep-alive');
          }
        },
        onError: (err, req, res) => {
          console.error('SSE Proxy error:', err);
        },
      },
    ],
  },
};
