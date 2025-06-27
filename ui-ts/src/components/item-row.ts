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
  @property({ type: Object }) tags: Record<string, string> = {};
  
  @state() private previousValue: any = null;
  @state() private isHighlighted = false;

  static styles = css`
    :host{display:block}
    .row{display:flex;gap:1rem;padding:.4rem 1rem;border-bottom:1px solid #ddd;align-items:flex-start}
    .row:nth-child(odd){background:#fafafa}
    .row span:first-child{flex:0 0 260px;font-weight:600}
    .row span:nth-child(2){flex:1 1 auto}
    .timestamp-info            { flex:0 0 200px; }
    .timestamp-text            { color:#888; }
    .timestamp-info .timestamp-label{
      display:block;
      font-size:.85em;
      color:#888;
      margin-top:.15em;
    }
    button {
      padding: .2rem .6rem;
      font-size: .9rem;
      cursor: pointer;
      min-width: 80px;   /* same width for both buttons */
    }
    .button-column {
      flex: 0 0 80px;
      display: flex;
      justify-content: center;
    }
    .bool-on  { color:#2e7d32; font-weight:600; }   /* green */
    .bool-off { color:#b8860b; font-weight:600; }   /* dark yellow */
    .row.highlighted {
      background: #e8f5e8 !important;
      transition: background-color 0.5s ease-in-out;
    }
    .value-changed {
      animation: flash 0.5s ease-in-out;
    }
    @keyframes flash {
      0% { background-color: #4caf50; }
      100% { background-color: transparent; }
    }
    .item-info .item-label{
      display:block;
      font-size:.85em;
      font-weight:400;
      color:#666;
      margin-top:.15em;
    }
    .value-info .type-hint{
      display:block;
      font-size:.85em;
      color:#888;
      margin-top:.15em;
    }

    /* Mobile responsive */
    @media (max-width: 768px) {
      .row {
        flex-direction: column;
        gap: 0.5rem;
        padding: 0.8rem 1rem;
      }
      
      .row span {
        flex: none !important;
        width: 100%;
      }
      
      .button-column {
        align-self: flex-end;
        margin-top: 0.5rem;
      }
    }
  `;

  updated(changedProperties: Map<string, any>) {
    if (changedProperties.has('value')) {
      const newValue = this.value;
      const oldValue = changedProperties.get('value');
      
      // Only highlight if the value actually changed and this isn't the initial render
      if (oldValue !== undefined && oldValue !== newValue) {
        this.highlightChange();
      }
    }
  }

  private highlightChange() {
    this.isHighlighted = true;
    
    // Remove highlight after a short delay
    setTimeout(() => {
      this.isHighlighted = false;
    }, 2000);
  }

  render() {
    const isReadonly = this.tags['ext-readonly'] === true || this.tags['ext-readonly'] === 't' || this.tags['ext-readonly'] === 'true';
    
    // Debug logging
    if (this.id === 'event-1') {
      console.log('EVENT-1 debug:', {
        id: this.id,
        tags: this.tags,
        extReadonly: this.tags['ext-readonly'],
        isReadonly: isReadonly
      });
    }
    
    return html`
      <div class="row ${this.isHighlighted ? 'highlighted' : ''}">
        <span class="item-info">
          <span class="item-name">${this.id}</span>
          ${this.label ? html`<span class="item-label">${this.label}</span>` : ''}
        </span>
        <span class="value-info">
          <span class=${`value-text ${this.booleanClass(this.value)}`}>${this.format(this.value)}</span>
          ${this.typeHint
            ? html`<span class="type-hint">${this.formatTypeHint(this.typeHint)}</span>`
            : ''}
        </span>
        <span class="timestamp-info">
          <span class="timestamp-text">${this.formatTimestamp(this.timestamp)}</span>
          <span class="timestamp-label">Last change</span>
        </span>
        <span class="button-column">
          ${!isReadonly ? html`
            <button @click=${this.changeValue}>
              ${typeof this.value === 'boolean' ? 'Toggle' : 'Change'}
            </button>
          ` : ''}
        </span>
      </div>
    `;
  }

  private format(v: unknown) {
    if (v === null) return 'null';

    /* real booleans */
    if (typeof v === 'boolean') return v ? 'On' : 'Off';

    /* string booleans ("true", "false", "on", "off", case-insensitive) */
    if (typeof v === 'string') {
      const l = v.toLowerCase();
      if (l === 'true'  || l === 'on')  return 'On';
      if (l === 'false' || l === 'off') return 'Off';
    }

    if (typeof v === 'object') return JSON.stringify(v);
    return String(v);
  }

  private booleanClass(v: unknown): string {
    if (typeof v === 'boolean') return v ? 'bool-on' : 'bool-off';
    if (typeof v === 'string') {
      const l = v.toLowerCase();
      if (l === 'true' || l === 'on')  return 'bool-on';
      if (l === 'false' || l === 'off') return 'bool-off';
    }
    return '';
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
      case 'boolean': return 'Switch';          // new
      case 'string':  return 'String';       // new
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
      // this.value = newVal; // don't set locally, value is supplied from outside
      this.dispatchEvent(new CustomEvent('item-updated', { bubbles: true, composed: true }));
    } catch (e: any) {
      if (e?.response?.status === 401) {
        this.dispatchEvent(new CustomEvent('need-auth', { bubbles: true, composed: true }));
      } else if (e?.response?.status === 403) {
        alert('Insufficient access rights.');
        // No need-auth event here!
      } else {
        alert('Update failed – check API-Key and server logs.');
      }
    }
  }
}
