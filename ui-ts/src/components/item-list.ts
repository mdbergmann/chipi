import { LitElement, html, css } from 'lit';
import { customElement, state } from 'lit/decorators.js';
import { fetchItemgroups } from '../api';
import './item-row';

interface Item { name: string; label?: string; value: any; typeHint?: string; timestamp?: number; tags?: Record<string, string>; }
interface Itemgroup {
  name: string; label?: string;
  items: Item[];
}

@customElement('item-list')
export class ItemList extends LitElement {
  @state() private groups: Itemgroup[] = [];
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
    .toolbar button:first-child { margin-right: .7rem; }

    /* Mobile responsive toolbar */
    @media (max-width: 768px) {
      .toolbar {
        padding: 0.5rem;
        justify-content: center;
        gap: 0.5rem;
      }
      
      .toolbar button {
        flex: 1;
        max-width: 120px;
      }
      
      .toolbar button:first-child { 
        margin-right: 0; 
      }
    }
    .empty{
      padding:1rem;
      text-align:center;
      color:#555;
    }
    .group-header{
      margin:0;
      padding:.6rem 1rem;
      background:#e0e0e0;
      border-top:1px solid #ccc;
      font-weight:600;
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
      /* fetchItemgroups now always returns an array */
      const apiGroups = await fetchItemgroups();
      this.groups = apiGroups
        .map((g: any) => ({
          name:  g.name,
          label: g.label,
          items: (Array.isArray(g.items) ? g.items : []).map((i: any) => ({
            ...i,
            typeHint: i['type-hint'] ?? i.typeHint,
            value: i['item-state']?.value,
            timestamp: i['item-state']?.timestamp,
            tags: i.tags || {}
          }))
        }))
        .filter(g => g.items.length > 0);
    } catch (e: any) {
      if (e?.response?.status === 401) {
        this.dispatchEvent(new CustomEvent('need-auth', { bubbles: true, composed: true }));
      } else if (e?.response?.status === 403) {
        this.error = 'Insufficient access rights.';
        this.dispatchEvent(
          new CustomEvent('need-auth',
                          { bubbles: true, composed: true,
                            detail: { message: 'API key does not have sufficient rights.' } })
        );
      } else {
        /* Axios error without response  ⇒ network/proxy issue.
           All other TypeError etc.      ⇒ invalid server JSON. */
        if (!e?.response) {
          this.error =
            e instanceof TypeError
              ? 'Invalid server response.'
              : 'API server is not reachable.';
        } else {                              // any other (unexpected) HTTP status
          const { status, statusText } = e.response;
          this.error = `Server error: ${status}${statusText ? ' – ' + statusText : ''}`;
        }
        this.groups = [];
      }
    }
  }

  render() {
    return html`
      <div class="toolbar">
        <button @click=${this.setApiKey}>Set API-Key</button>
        <button @click=${() => this.load()}>Refresh</button>
      </div>
      ${this.error
        ? html`<div class="error">${this.error}</div>`
        : this.groups.length === 0
          ? html`<div class="empty">No itemgroups available.</div>`
          : html`${[...this.groups].reverse().map(
              g => html`
                <h3 class="group-header">${g.label ?? g.name}</h3>
                ${g.items.map(i => html`
                  <item-row
                    .id=${i.name}
                    .label=${i.label ?? ''}
                    .value=${i.value}
                    .typeHint=${i.typeHint}
                    .timestamp=${i.timestamp}
                    .tags=${i.tags || {}}
                    @item-updated=${() => this.load()}>
                  </item-row>`)}
              `)}`
      }
    `;
  }

  private setApiKey() {
    this.dispatchEvent(new CustomEvent('need-auth', { bubbles: true, composed: true }));
  }
}
