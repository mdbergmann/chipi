import { LitElement, html, css } from 'lit';
import { customElement, property, state } from 'lit/decorators.js';
import { updateItem } from '../api';

@customElement('item-row')
export class ItemRow extends LitElement {
  @property({ type: String }) id = '';
  @property({ type: String }) label = '';
  @state() value: any = null;

  static styles = css`
    :host{display:block}
    .row{display:flex;gap:1rem;padding:.4rem 1rem;border-bottom:1px solid #ddd;align-items:center}
    .row:nth-child(odd){background:#fafafa}
    .row span:first-child{flex:0 0 160px;font-weight:600}
    .row span:nth-child(2){flex:1 1 auto}
    button{padding:.2rem .6rem;font-size:.9rem;cursor:pointer}
  `;

  render() {
    return html`
      <div class="row">
        <span>${this.id}</span>
        <span>${this.format(this.value)}</span>
        <button @click=${this.changeValue}>Change</button>
      </div>
    `;
  }

  private format(v: unknown) {
    if (v === null) return 'null';
    if (typeof v === 'object') return JSON.stringify(v);
    return String(v);
  }

  private async changeValue() {
    let newVal: unknown = this.value;

    if (typeof this.value === 'boolean') {
      newVal = !this.value;
    } else {
      const input = prompt(`New value for ${this.id}`, this.format(this.value));
      if (input === null) return;
      if (typeof this.value === 'number') newVal = Number(input);
      else newVal = input.replace(/^"(.*)"$/, '$1'); // strip surrounding quotes
    }

    try {
      await updateItem(this.id, newVal);
      this.value = newVal;
    } catch (e: any) {
      if (e?.response?.status === 401) {
        this.dispatchEvent(new CustomEvent('need-auth', { bubbles: true, composed: true }));
      } else if (e?.response?.status === 403) {
        alert('Insufficient access rights.');
        this.dispatchEvent(new CustomEvent('need-auth', { bubbles: true, composed: true }));
      } else {
        alert('Update failed – check API-Key and server logs.');
      }
    }
  }
}
