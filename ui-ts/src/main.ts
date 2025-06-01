import { getApiKey, clearApiKey } from './storage';
import './components/api-key-dialog';
import './components/item-list';

const mount = document.getElementById('app')!;

function render() {
  mount.innerHTML = '';
  mount.appendChild(document.createElement(getApiKey() ? 'item-list'
                                                       : 'api-key-dialog'));
}

render();
window.addEventListener('api-key-set', render);
window.addEventListener('need-auth', () => { clearApiKey(); render(); });
