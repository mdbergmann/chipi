import './styles.css';
import { ChipiApiClient } from './api/client';
import { SSEEvent, FlexibleSSEEvent } from './api/types';
import { loadApiKey } from './utils/storage';
import { Toolbar } from './components/Toolbar';
import { ApiKeyDialog } from './components/ApiKeyDialog';
import { ItemGroupComponent } from './components/ItemGroupComponent';

class ChipiApp {
    private apiClient: ChipiApiClient;
    private toolbar!: Toolbar;
    private apiKeyDialog!: ApiKeyDialog;
    private itemGroupComponents: ItemGroupComponent[] = [];
    private eventSource: EventSource | null = null;

    constructor() {
        // Für SSE: direkte API-Verbindung verwenden
        this.apiClient = new ChipiApiClient('http://localhost:8765');
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
            console.error('EventSource konnte nicht erstellt werden');
            return;
        }

        this.eventSource.onmessage = (event) => {
            console.log('SSE Rohdaten empfangen:', event.data); // Debug-Log
            console.log('SSE Event Type:', event.type); // Debug-Log
            try {
                const data = JSON.parse(event.data);
                console.log('SSE Parsed Data:', data); // Debug-Log
                this.handleSSEEvent(data);
            } catch (error) {
                console.error('Fehler beim Parsen der SSE-Nachricht:', error, 'Rohdaten:', event.data);
                // Versuche alternative Parsing-Strategien
                this.handleRawSSEEvent(event.data);
            }
        };

        this.eventSource.onerror = (error) => {
            console.error('SSE-Verbindungsfehler:', error);
            console.log('EventSource readyState:', this.eventSource?.readyState);
            console.log('EventSource URL:', this.eventSource?.url);
        };

        this.eventSource.onopen = () => {
            console.log('SSE-Verbindung hergestellt');
            console.log('EventSource URL:', this.eventSource?.url);
        };
    }

    private handleSSEEvent(data: any): void {
        console.log('SSE Event empfangen:', data); // Debug-Log
        
        // Versuche verschiedene Event-Strukturen
        let eventType: string;
        let eventData: any;
        
        if (data.event) {
            // Struktur: { event: { type: "...", ... } }
            eventType = data.event.type;
            eventData = data.event;
        } else if (data.type) {
            // Struktur: { type: "...", ... }
            eventType = data.type;
            eventData = data;
        } else {
            console.log('Unbekannte SSE Event-Struktur:', data);
            return;
        }
        
        console.log('Event Type:', eventType);
        
        switch (eventType) {
            case 'connection':
                console.log('Connection Event empfangen:', eventData.message || 'Verbindung hergestellt');
                break;
                
            case 'heartbeat':
                console.log('Heartbeat Event empfangen, Timestamp:', eventData.timestamp);
                break;
                
            case 'item-change':
                console.log('Item-Change Event empfangen für:', eventData.item?.name);
                if (eventData.item) {
                    this.itemGroupComponents.forEach(component => {
                        component.updateItem(eventData.item);
                    });
                } else {
                    console.error('Item-Change Event ohne Item-Daten:', eventData);
                }
                break;
                
            default:
                console.log('Unbekannter Event Type:', eventType, 'Data:', eventData);
        }
    }

    private handleRawSSEEvent(rawData: string): void {
        console.log('Versuche Raw SSE Event zu verarbeiten:', rawData);
        
        // Manchmal kommen Events als einfache Strings
        if (rawData.includes('connection') || rawData.includes('heartbeat') || rawData.includes('item-change')) {
            console.log('Raw Event erkannt:', rawData);
        }
    }
}

// Initialize the app when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new ChipiApp();
});
