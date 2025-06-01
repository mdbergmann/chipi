import { LitElement, html, css } from 'lit';
import { customElement, state } from 'lit/decorators.js';
import { fetchItems } from '../api';
import './item-row';

interface Item { name: string; label?: string; value: any; }

@customElement('item-list')
export class ItemList extends LitElement {
  @state() private items: Item[] = [];
  @state() private error: string | null = null;

  static styles = css`
    .error {
      padding: 1rem;
      background: #c62828;
      color: #fff;
      text-align: center;
    }
  `;

  connectedCallback() {
    super.connectedCallback();
    this.load();
    this.addEventListener('need-auth', () => this.requestUpdate());
    // refresh every 10 s
    setInterval(() => this.load(), 10_000);
  }

  private async load() {
    try {
      this.error = null;          // clear previous error
      this.items = await fetchItems();
    } catch (e: any) {
      if (e?.response?.status === 401) {
        this.dispatchEvent(new CustomEvent('need-auth', { bubbles: true, composed: true }));
      } else {
        // no response ⇒ server not reachable
        this.error = 'API server is not reachable.';
        this.items = [];
      }
    }
  }

  render() {
    if (this.error) {
      return html`<div class="error">${this.error}</div>`;
    }
    return html`${this.items.map(
      i => html`<item-row .id=${i.name} .value=${i.value}></item-row>`
    )}`;
  }
}
