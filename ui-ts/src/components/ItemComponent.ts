import { Item } from '../api/types';

export class ItemComponent {
    private element: HTMLElement;
    private item: Item;
    private onUpdate: (name: string, value: any) => void;

    constructor(item: Item, onUpdate: (name: string, value: any) => void) {
        this.item = item;
        this.onUpdate = onUpdate;
        this.element = this.createElement();
    }

    private createElement(): HTMLElement {
        const itemDiv = document.createElement('div');
        itemDiv.className = 'item';
        
        const infoDiv = document.createElement('div');
        infoDiv.className = 'item-info';
        
        const nameDiv = document.createElement('div');
        nameDiv.className = 'item-name';
        nameDiv.textContent = this.item.name;
        
        const labelDiv = document.createElement('div');
        labelDiv.className = 'item-label';
        labelDiv.textContent = this.item.label;
        
        const metaDiv = document.createElement('div');
        metaDiv.className = 'item-meta';
        metaDiv.textContent = this.getMetaText();
        
        infoDiv.appendChild(nameDiv);
        infoDiv.appendChild(labelDiv);
        infoDiv.appendChild(metaDiv);
        
        const controlDiv = document.createElement('div');
        controlDiv.className = 'item-control';
        
        if (this.isReadOnly()) {
            controlDiv.classList.add('readonly');
        }
        
        this.createControl(controlDiv);
        
        itemDiv.appendChild(infoDiv);
        itemDiv.appendChild(controlDiv);
        
        return itemDiv;
    }

    private getMetaText(): string {
        const timestamp = new Date((this.item['item-state'].timestamp - 2208988800) * 1000);
        const typeText = this.getTypeText();
        return `${typeText} • ${timestamp.toLocaleString('de-DE')}`;
    }

    private getTypeText(): string {
        switch (this.item['type-hint']) {
            case 'boolean': return 'Ein/Aus';
            case 'float': return 'Dezimalzahl';
            case 'integer': return 'Ganzzahl';
            case 'string': return 'Text';
            default: return this.item['type-hint'];
        }
    }

    private isReadOnly(): boolean {
        return this.item.tags[':ext-readonly'] === true;
    }

    private createControl(container: HTMLElement): void {
        const value = this.item['item-state'].value;
        
        if (this.item['type-hint'] === 'boolean') {
            this.createBooleanControl(container, value);
        } else {
            this.createValueControl(container, value);
        }
    }

    private createBooleanControl(container: HTMLElement, value: boolean): void {
        const statusSpan = document.createElement('span');
        statusSpan.className = value ? 'status-on' : 'status-off';
        statusSpan.textContent = value ? 'Ein' : 'Aus';
        
        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.checked = value;
        checkbox.disabled = this.isReadOnly();
        
        checkbox.addEventListener('change', () => {
            this.onUpdate(this.item.name, checkbox.checked);
        });
        
        container.appendChild(statusSpan);
        container.appendChild(checkbox);
    }

    private createValueControl(container: HTMLElement, value: any): void {
        const input = document.createElement('input');
        input.type = this.item['type-hint'] === 'float' || this.item['type-hint'] === 'integer' ? 'number' : 'text';
        input.value = String(value);
        input.disabled = this.isReadOnly();
        
        if (this.item['type-hint'] === 'integer') {
            input.step = '1';
        } else if (this.item['type-hint'] === 'float') {
            input.step = 'any';
        }
        
        input.addEventListener('change', () => {
            let newValue: any = input.value;
            
            if (this.item['type-hint'] === 'integer') {
                newValue = parseInt(newValue, 10);
            } else if (this.item['type-hint'] === 'float') {
                newValue = parseFloat(newValue);
            }
            
            this.onUpdate(this.item.name, newValue);
        });
        
        container.appendChild(input);
    }

    updateItem(newItem: Item): void {
        this.item = newItem;
        
        // Update meta text
        const metaDiv = this.element.querySelector('.item-meta') as HTMLElement;
        metaDiv.textContent = this.getMetaText();
        
        // Update control
        const controlDiv = this.element.querySelector('.item-control') as HTMLElement;
        controlDiv.innerHTML = '';
        
        if (this.isReadOnly()) {
            controlDiv.classList.add('readonly');
        } else {
            controlDiv.classList.remove('readonly');
        }
        
        this.createControl(controlDiv);
    }

    getElement(): HTMLElement {
        return this.element;
    }
}
