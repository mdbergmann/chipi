import { LitElement, html } from 'lit';
import { customElement, state } from 'lit/decorators.js';
import { fetchItems } from '../api';
import './item-row';

interface Item { name: string; label?: string; value: any; }

@customElement('item-list')
export class ItemList extends LitElement {
  @state() private items: Item[] = [];

  connectedCallback() {
    super.connectedCallback();
    this.load();
    this.addEventListener('need-auth', () => this.requestUpdate());
    // refresh every 10 s
    setInterval(() => this.load(), 10_000);
  }

  private async load() {
    try {
      this.items = await fetchItems();
    } catch (e: any) {
      if (e?.response?.status === 401) {
        this.dispatchEvent(new CustomEvent('need-auth', { bubbles: true, composed: true }));
      }
    }
  }

  render() {
    return html`${this.items.map(i =>
      html`<item-row .id=${i.name} .value=${i.value}></item-row>`)}`
  }
}
