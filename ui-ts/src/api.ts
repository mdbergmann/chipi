import axios from 'axios';
import { getApiKey } from './storage';

const api = axios.create({
  baseURL: '',                    // relative → gleicher Origin / Dev-Proxy
  headers: { Accept: 'application/json' }
});

api.interceptors.request.use(cfg => {
  const key = getApiKey();
  if (key) cfg.headers['X-Api-Key'] = key;
  return cfg;
});

api.interceptors.response.use(
  res => res,
  err => {
    if (err?.response?.status === 401) {
      window.dispatchEvent(new CustomEvent('need-auth'));
    }
    // Kein need-auth Event mehr bei 403!
    return Promise.reject(err);
  }
);

export const fetchItemgroups = () =>
  api.get('/itemgroups').then(r => r.data);   // GET /itemgroups

export const updateItem  = (id: string, value: unknown) =>
  api.post(`/items/${id}`, { value }).then(r => r.data);

export default api;
