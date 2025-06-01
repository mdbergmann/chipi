import { LitElement, html, css } from 'lit';
import { customElement, state, property } from 'lit/decorators.js';
import { setApiKey } from '../storage';

@customElement('api-key-dialog')
export class ApiKeyDialog extends LitElement {
  @state() private key = '';
  @property({ type: String }) error: string | null = null;

  static styles = css`
    :host{position:fixed;inset:0;display:grid;place-items:center;background:rgba(0,0,0,.35)}
    .box{background:#fff;padding:1.5rem;border-radius:6px;min-width:260px;box-shadow:0 4px 20px rgba(0,0,0,.25)}
    .error{color:#c62828;margin-bottom:.6rem;}
    input{width:100%;margin:.5rem 0;padding:.4rem;font:inherit}
    button{padding:.3rem .8rem;font:inherit;cursor:pointer}
  `;

  render() {
    return html`
      <div class="box">
        ${this.error ? html`<p class="error">${this.error}</p>` : null}
        <h3>Enter API-Key</h3>
        <input
          placeholder="API-Key"
          @input=${(e: InputEvent) => { this.key = (e.target as HTMLInputElement).value.trim(); }}
          @keyup=${(e: KeyboardEvent) => { if (e.key === 'Enter') this.save(); }} />
        <button @click=${this.save}>Save</button>
      </div>
    `;
  }

  private save() {
    if (!this.key) return;
    setApiKey(this.key);
    this.dispatchEvent(new CustomEvent('api-key-set', { bubbles: true, composed: true }));
    this.remove();
  }
}
