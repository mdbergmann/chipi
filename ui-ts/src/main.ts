import { clearApiKey } from './storage';
import './components/api-key-dialog';
import './components/item-list';

const mount = document.getElementById('app');
if (!mount) {
  throw new Error('Element with ID "app" not found in the DOM.');
}

function render(component: 'item-list' | 'api-key-dialog' = 'item-list') {
  mount.innerHTML = '';
  mount.appendChild(document.createElement(component));
}

render();                                           // initialer Start mit Item-Liste
window.addEventListener('api-key-set', () => render());
window.addEventListener('need-auth', () => {
  clearApiKey();
  render('api-key-dialog');                         // erst danach Dialog anzeigen
});
