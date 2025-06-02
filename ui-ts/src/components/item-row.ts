import { LitElement, html, css } from 'lit';
import { customElement, property, state } from 'lit/decorators.js';
import { updateItem } from '../api';

@customElement('item-row')
export class ItemRow extends LitElement {
  @property({ type: String }) id = '';
  @property({ type: String }) label = '';
  @property({ type: String }) typeHint = '';
  @property({ type: Object }) value: any = null;
  @property({ type: Number }) timestamp?: number;

  static styles = css`
    :host{display:block}
    .row{display:flex;gap:1rem;padding:.4rem 1rem;border-bottom:1px solid #ddd;align-items:center}
    .row:nth-child(odd){background:#fafafa}
    .row span:first-child{flex:0 0 260px;font-weight:600}
    .row span:nth-child(2){flex:1 1 auto}
    .row span:nth-child(3){flex:0 0 120px;text-align:right;color:#888;}
    .row span:nth-child(4){flex:0 0 160px;text-align:right;color:#888;}
    button {
      padding: .2rem .6rem;
      font-size: .9rem;
      cursor: pointer;
      min-width: 80px;   /* <--- NEU: gleiche Breite für beide Buttons */
    }
  `;

  render() {
    return html`
      <div class="row">
        <span>
          ${this.id}
          ${this.label ? html`<span style="color:#888;font-weight:400;margin-left:.5em">(${this.label})</span>` : ''}
        </span>
        <span>${this.format(this.value)}</span>
        <span style="color:#888;font-size:.95em">${this.formatTypeHint(this.typeHint)}</span>
        <span style="color:#888;font-size:.95em">${this.formatTimestamp(this.timestamp)}</span>
        <button @click=${this.changeValue}>
          ${typeof this.value === 'boolean' ? 'Toggle' : 'Change'}
        </button>
      </div>
    `;
  }

  private format(v: unknown) {
    if (v === null) return 'null';

    /* echte Booleans */
    if (typeof v === 'boolean') return v ? 'On' : 'Off';

    /* String-Booleans („true“, „false“, „on“, „off“, gemischte Groß-/Kleinschreibung) */
    if (typeof v === 'string') {
      const l = v.toLowerCase();
      if (l === 'true'  || l === 'on')  return 'On';
      if (l === 'false' || l === 'off') return 'Off';
    }

    if (typeof v === 'object') return JSON.stringify(v);
    return String(v);
  }

  private formatTimestamp(ts?: number): string {
    if (!ts) return '';
    // Automatische Erkennung: Sekunden oder Millisekunden
    const ms = ts > 1e12 ? ts : ts * 1000;
    const d = new Date(ms);
    return d.toLocaleString();
  }

  private formatTypeHint(t?: string): string {
    if (!t) return '';
    switch (t.toLowerCase()) {
      case 'integer': return 'Whole Number';
      case 'float':   return 'Decimal Number';
      default:        return t;
    }
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
      // this.value = newVal; // nicht lokal setzen, da value von außen kommt
      this.dispatchEvent(new CustomEvent('item-updated', { bubbles: true, composed: true }));
    } catch (e: any) {
      if (e?.response?.status === 401) {
        this.dispatchEvent(new CustomEvent('need-auth', { bubbles: true, composed: true }));
      } else if (e?.response?.status === 403) {
        alert('Insufficient access rights.');
        // Kein need-auth Event mehr!
      } else {
        alert('Update failed – check API-Key and server logs.');
      }
    }
  }
}
