import axios from 'axios';
import { getApiKey } from './storage';

const api = axios.create({
  baseURL: '',                    // relative → same origin / dev proxy
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
    // No need-auth event anymore for 403!
    return Promise.reject(err);
  }
);

/* Backend may return either an array or an object like
   { itemgroups : [...] }.  Accept both variants. */
export const fetchItemgroups = () =>
  api.get('/itemgroups').then(r => {
    const d = r.data;
    return Array.isArray(d)              ? d
         : d?.itemgroups === false       ? []            // treat explicit `false` as empty list
         : Array.isArray(d?.itemgroups)  ? d.itemgroups
         : Array.isArray(d?.itemGroups)  ? d.itemGroups  // Fallback Groß/Klein
         : [];                                           // unsupported format
  });

export const updateItem  = (id: string, value: unknown) =>
  api.post(`/items/${id}`, { value }).then(r => r.data);

export default api;
