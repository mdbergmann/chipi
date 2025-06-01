import { LitElement, html, css } from 'lit';
import { customElement, state } from 'lit/decorators.js';
import { fetchItems } from '../api';
import './item-row';

interface Item { name: string; label?: string; value: any; typeHint?: string; }

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
    .toolbar{
      padding:.5rem 1rem;
      background:#f0f0f0;
      border-bottom:1px solid #ddd;
      display:flex;
      justify-content:flex-end;
    }
    .toolbar button{
      padding:.3rem .8rem;
      font:inherit;
      cursor:pointer;
    }
    .empty{
      padding:1rem;
      text-align:center;
      color:#555;
    }
  `;

  connectedCallback() {
    super.connectedCallback();
    this.load().catch(console.error);
    this.addEventListener('need-auth', () => this.requestUpdate());
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

  render() {
    return html`
      <div class="toolbar">
        <button @click=${() => this.load()}>Refresh</button>
      </div>
      ${this.error
        ? html`<div class="error">${this.error}</div>`
        : this.items.length === 0
          ? html`<div class="empty">No items available.</div>`
          : html`${this.items.map(
              i => html`<item-row .id=${i.name} .value=${i.value} .typeHint=${i.typeHint}></item-row>`
            )}`
      }
    `;
  }
}
