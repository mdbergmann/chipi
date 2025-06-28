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
        // Für Entwicklung: leeren baseUrl verwenden (nutzt Webpack Proxy)
        this.apiClient = new ChipiApiClient('');
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
        console.log('=== setApiKey aufgerufen ===');
        this.apiClient.setApiKey(apiKey);
        this.logNetworkInfo();
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
        console.log('=== SSE Setup gestartet ===');
        
        if (this.eventSource) {
            console.log('Schließe bestehende EventSource...');
            this.eventSource.close();
            this.eventSource = null;
        }

        console.log('Erstelle neue EventSource...');
        this.eventSource = this.apiClient.createEventSource();
        
        if (!this.eventSource) {
            console.error('EventSource konnte nicht erstellt werden - kein API-Key?');
            return;
        }

        console.log('EventSource erstellt:', this.eventSource);
        console.log('EventSource URL:', this.eventSource.url);
        console.log('EventSource readyState:', this.eventSource.readyState);

        // Alle möglichen Event-Listener
        this.eventSource.addEventListener('open', (event) => {
            console.log('=== SSE OPEN EVENT ===');
            console.log('Event:', event);
            console.log('EventSource readyState nach open:', this.eventSource?.readyState);
            console.log('EventSource URL:', this.eventSource?.url);
        });

        this.eventSource.addEventListener('message', (event) => {
            console.log('=== SSE MESSAGE EVENT ===');
            console.log('Event type:', event.type);
            console.log('Event data:', event.data);
            console.log('Event lastEventId:', event.lastEventId);
            console.log('Event origin:', event.origin);
            this.handleSSEMessage(event.data);
        });

        this.eventSource.addEventListener('error', (event) => {
            console.log('=== SSE ERROR EVENT ===');
            console.error('SSE-Fehler Event:', event);
            console.log('EventSource readyState bei Fehler:', this.eventSource?.readyState);
            console.log('EventSource URL:', this.eventSource?.url);
            
            // ReadyState bedeutungen loggen
            if (this.eventSource) {
                switch (this.eventSource.readyState) {
                    case EventSource.CONNECTING:
                        console.log('Status: CONNECTING (0) - Verbindung wird aufgebaut');
                        break;
                    case EventSource.OPEN:
                        console.log('Status: OPEN (1) - Verbindung ist offen');
                        break;
                    case EventSource.CLOSED:
                        console.log('Status: CLOSED (2) - Verbindung ist geschlossen');
                        break;
                    default:
                        console.log('Status: Unbekannt -', this.eventSource.readyState);
                }
            }
            
            // Automatischer Reconnect nach Fehler
            if (this.eventSource?.readyState === EventSource.CLOSED) {
                console.log('SSE-Verbindung geschlossen, versuche Reconnect in 5 Sekunden...');
                setTimeout(() => {
                    console.log('Starte SSE Reconnect...');
                    this.setupSSE();
                }, 5000);
            }
        });

        // Fallback onmessage
        this.eventSource.onmessage = (event) => {
            console.log('=== SSE ONMESSAGE FALLBACK ===');
            console.log('Event data:', event.data);
            this.handleSSEMessage(event.data);
        };

        // Fallback onerror
        this.eventSource.onerror = (event) => {
            console.log('=== SSE ONERROR FALLBACK ===');
            console.error('SSE onerror fallback:', event);
        };

        // Fallback onopen
        this.eventSource.onopen = (event) => {
            console.log('=== SSE ONOPEN FALLBACK ===');
            console.log('SSE onopen fallback:', event);
        };

        // Status nach kurzer Zeit prüfen
        setTimeout(() => {
            console.log('=== SSE Status Check nach 2 Sekunden ===');
            if (this.eventSource) {
                console.log('EventSource readyState:', this.eventSource.readyState);
                console.log('EventSource URL:', this.eventSource.url);
            } else {
                console.log('EventSource ist null');
            }
        }, 2000);

        console.log('=== SSE Setup abgeschlossen ===');
    }

    private handleSSEMessage(data: string): void {
        console.log('SSE Rohdaten empfangen:', data);
        try {
            const parsed = JSON.parse(data);
            console.log('SSE Parsed Data:', parsed);
            this.handleSSEEvent(parsed);
        } catch (error) {
            console.error('Fehler beim Parsen der SSE-Nachricht:', error, 'Rohdaten:', data);
            this.handleRawSSEEvent(data);
        }
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

    private logNetworkInfo(): void {
        console.log('=== Network Debug Info ===');
        console.log('Aktuelle URL:', window.location.href);
        console.log('Origin:', window.location.origin);
        console.log('User Agent:', navigator.userAgent);
        console.log('Online Status:', navigator.onLine);
        
        // Test ob der API-Server erreichbar ist
        fetch('http://localhost:8765/items', {
            method: 'HEAD',
            headers: this.apiClient['getHeaders'](),
        }).then(response => {
            console.log('API-Server HEAD Request Status:', response.status);
            console.log('API-Server erreichbar:', response.ok);
        }).catch(error => {
            console.error('API-Server nicht erreichbar:', error);
        });
    }
}

// Initialize the app when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new ChipiApp();
});
