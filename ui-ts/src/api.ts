import axios from 'axios';
import { getApiKey } from './storage';

const api = axios.create({
  baseURL: 'http://localhost:8765',
  headers: { Accept: 'application/json' }
});

api.interceptors.request.use(cfg => {
  const key = getApiKey();
  if (key) cfg.headers['X-Api-Key'] = key;
  return cfg;
});

export const fetchItems  = () => api.get('/items').then(r => r.data);
export const updateItem  = (id: string, value: unknown) =>
  api.post(`/items/${id}`, { value }).then(r => r.data);

export default api;
