import './styles.css';
import { ChipiApiClient } from './api/client';
import { SSEEvent } from './api/types';
import { loadApiKey } from './utils/storage';
import { Toolbar } from './components/Toolbar';
import { ApiKeyDialog } from './components/ApiKeyDialog';
import { ItemGroupComponent } from './components/ItemGroupComponent';

class ChipiApp {
    private apiClient: ChipiApiClient;
    private toolbar: Toolbar;
    private apiKeyDialog: ApiKeyDialog;
    private itemGroupComponents: ItemGroupComponent[] = [];
    private eventSource: EventSource | null = null;

    constructor() {
        this.apiClient = new ChipiApiClient();
        this.initializeUI();
        this.loadInitialApiKey();
    }

    private initializeUI(): void {
        const toolbarContainer = document.getElementById('toolbar')!;
        this.toolbar = new Toolbar(toolbarContainer, () => this.showApiKeyDialog());
        
        this.apiKeyDialog = new ApiKeyDialog((apiKey) => this.setApiKey(apiKey));
    }

    private loadInitialApiKey(): void {
        const savedApiKey = loadApiKey();
        if (savedApiKey) {
            this.setApiKey(savedApiKey);
        } else {
            this.showApiKeyDialog();
        }
    }

    private showApiKeyDialog(): void {
        this.apiKeyDialog.show();
    }

    private setApiKey(apiKey: string): void {
        this.apiClient.setApiKey(apiKey);
        this.loadData();
        this.setupSSE();
    }

    private async loadData(): Promise<void> {
        const loadingEl = document.getElementById('loading')!;
        const errorEl = document.getElementById('error')!;
        const itemgroupsEl = document.getElementById('itemgroups')!;

        try {
            loadingEl.classList.remove('hidden');
            errorEl.classList.add('hidden');
            itemgroupsEl.innerHTML = '';

            const itemGroups = await this.apiClient.getItemGroups();
            
            this.itemGroupComponents = itemGroups.map(group => 
                new ItemGroupComponent(group, (name, value) => this.updateItem(name, value))
            );

            this.itemGroupComponents.forEach(component => {
                itemgroupsEl.appendChild(component.getElement());
            });

            loadingEl.classList.add('hidden');
        } catch (error) {
            loadingEl.classList.add('hidden');
            errorEl.textContent = `Fehler beim Laden der Daten: ${error}`;
            errorEl.classList.remove('hidden');
        }
    }

    private async updateItem(name: string, value: any): Promise<void> {
        try {
            await this.apiClient.updateItem(name, value);
        } catch (error) {
            const errorEl = document.getElementById('error')!;
            errorEl.textContent = `Fehler beim Aktualisieren von ${name}: ${error}`;
            errorEl.classList.remove('hidden');
            
            // Hide error after 5 seconds
            setTimeout(() => {
                errorEl.classList.add('hidden');
            }, 5000);
        }
    }

    private setupSSE(): void {
        if (this.eventSource) {
            this.eventSource.close();
        }

        this.eventSource = this.apiClient.createEventSource();
        if (!this.eventSource) {
            return;
        }

        this.eventSource.onmessage = (event) => {
            try {
                const data: SSEEvent = JSON.parse(event.data);
                this.handleSSEEvent(data);
            } catch (error) {
                console.error('Fehler beim Parsen der SSE-Nachricht:', error);
            }
        };

        this.eventSource.onerror = (error) => {
            console.error('SSE-Verbindungsfehler:', error);
        };

        this.eventSource.onopen = () => {
            console.log('SSE-Verbindung hergestellt');
        };
    }

    private handleSSEEvent(data: SSEEvent): void {
        if (data.event.type === 'item-change' && data.event.item) {
            this.itemGroupComponents.forEach(component => {
                component.updateItem(data.event.item);
            });
        }
    }
}

// Initialize the app when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new ChipiApp();
});
