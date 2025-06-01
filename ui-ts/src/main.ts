import { clearApiKey } from './storage';
import './components/api-key-dialog';
import './components/item-list';

const mount = document.getElementById('app');
if (!mount) {
  throw new Error('Element with ID "app" not found in the DOM.');
}

function render(component: 'item-list' | 'api-key-dialog' = 'item-list', message?: string) {
  mount.innerHTML = '';
  const el = document.createElement(component);
  if (component === 'api-key-dialog' && message) {
    // Property statt Attribut setzen → kein Attribut-Parsing nötig
    (el as any).error = message;
  }
  mount.appendChild(el);
}

render();                                           // initialer Start mit Item-Liste
window.addEventListener('api-key-set', () => render());
window.addEventListener('need-auth', (e) => {
  clearApiKey();
  render('api-key-dialog', (e as CustomEvent).detail?.message);
});
