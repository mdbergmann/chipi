import { LitElement, html, css } from 'lit';
import { customElement, state } from 'lit/decorators.js';
import { fetchItems } from '../api';
import './item-row';

interface Item { name: string; label?: string; value: any; }

@customElement('item-list')
export class ItemList extends LitElement {
  @state() private items: Item[] = [];
  @state() private error: string | null = null;
  private refreshId?: number;

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
    this.load().catch(console.error);
    this.addEventListener('need-auth', () => this.requestUpdate());
    // refresh every 10 s
    this.refreshId = window.setInterval(() => this.load(), 10_000);
  }

  private async load() {
    try {
      this.error = null;          // clear previous error
      this.items = await fetchItems();
    } catch (e: any) {
      if (e?.response?.status === 401) {
        this.dispatchEvent(new CustomEvent('need-auth', { bubbles: true, composed: true }));
      } else if (e?.response?.status === 403) {
        this.error = 'Insufficient access rights.';
        this.dispatchEvent(
          new CustomEvent('need-auth',
                          { bubbles: true, composed: true,
                            detail: { message: 'API-Key besitzt nicht ausreichende Rechte.' } })
        );
      } else {
        // no response ⇒ server not reachable
        this.error = 'API server is not reachable.';
        this.items = [];
      }
    }
  }

  disconnectedCallback() {
    super.disconnectedCallback();
    if (this.refreshId !== undefined) {
      clearInterval(this.refreshId);
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
