import { ItemGroup } from '../api/types';
import { ItemComponent } from './ItemComponent';

export class ItemGroupComponent {
    private element: HTMLElement;
    private itemGroup: ItemGroup;
    private itemComponents: Map<string, ItemComponent> = new Map();
    private onItemUpdate: (name: string, value: any) => void;

    constructor(itemGroup: ItemGroup, onItemUpdate: (name: string, value: any) => void) {
        this.itemGroup = itemGroup;
        this.onItemUpdate = onItemUpdate;
        this.element = this.createElement();
    }

    private createElement(): HTMLElement {
        const groupDiv = document.createElement('div');
        groupDiv.className = 'itemgroup';
        
        const headerDiv = document.createElement('div');
        headerDiv.className = 'itemgroup-header';
        headerDiv.textContent = this.itemGroup.label || this.itemGroup.name;
        
        const itemsDiv = document.createElement('div');
        itemsDiv.className = 'itemgroup-items';
        
        this.itemGroup.items.forEach(item => {
            const itemComponent = new ItemComponent(item, this.onItemUpdate);
            this.itemComponents.set(item.name, itemComponent);
            itemsDiv.appendChild(itemComponent.getElement());
        });
        
        groupDiv.appendChild(headerDiv);
        groupDiv.appendChild(itemsDiv);
        
        return groupDiv;
    }

    updateItem(item: any): void {
        const itemComponent = this.itemComponents.get(item.name);
        if (itemComponent) {
            itemComponent.updateItem(item);
        }
    }

    getElement(): HTMLElement {
        return this.element;
    }
}
