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

        // Nur die wichtigsten Event-Listener - keine Duplikate
        this.eventSource.onopen = (event) => {
            console.log('=== SSE VERBINDUNG GEÖFFNET ===');
            console.log('ReadyState:', this.eventSource?.readyState);
        };

        this.eventSource.onmessage = (event) => {
            console.log('=== SSE MESSAGE EMPFANGEN ===');
            console.log('Rohe Event-Daten:', event.data);
            console.log('Event Type:', event.type);
            console.log('Last Event ID:', event.lastEventId);
            
            // Versuche verschiedene Parsing-Strategien
            this.handleSSEMessage(event.data);
        };

        this.eventSource.onerror = (event) => {
            console.log('=== SSE FEHLER ===');
            console.error('Error Event:', event);
            console.log('ReadyState:', this.eventSource?.readyState);
            
            if (this.eventSource?.readyState === EventSource.CLOSED) {
                console.log('Verbindung geschlossen - Reconnect in 5 Sekunden');
                setTimeout(() => this.setupSSE(), 5000);
            }
        };

        console.log('=== SSE Setup abgeschlossen ===');
    }

    private handleSSEMessage(data: string): void {
        console.log('=== VERARBEITE SSE MESSAGE ===');
        console.log('Rohdaten:', data);
        console.log('Daten-Typ:', typeof data);
        console.log('Daten-Länge:', data.length);
        
        // Prüfe ob die Daten leer sind
        if (!data || data.trim() === '') {
            console.log('Leere SSE-Nachricht empfangen');
            return;
        }
        
        try {
            const parsed = JSON.parse(data);
            console.log('JSON erfolgreich geparst:', parsed);
            this.handleSSEEvent(parsed);
        } catch (error) {
            console.error('JSON Parse Fehler:', error);
            console.log('Versuche alternative Behandlung...');
            
            // Manchmal kommen Events in mehreren Zeilen
            const lines = data.split('\n');
            for (const line of lines) {
                if (line.trim().startsWith('{')) {
                    try {
                        const parsed = JSON.parse(line.trim());
                        console.log('JSON aus Zeile geparst:', parsed);
                        this.handleSSEEvent(parsed);
                        return;
                    } catch (e) {
                        console.log('Zeile konnte nicht geparst werden:', line);
                    }
                }
            }
            
            // Als letzter Ausweg: rohe Daten loggen
            console.log('Konnte Event nicht parsen, rohe Daten:', JSON.stringify(data));
        }
    }

    private handleSSEEvent(data: any): void {
        console.log('=== ANALYSIERE SSE EVENT ===');
        console.log('Event Objekt:', data);
        console.log('Event Keys:', Object.keys(data));
        
        // Logge alle möglichen Strukturen
        if (data.event) {
            console.log('Struktur: data.event gefunden');
            console.log('Event Type:', data.event.type);
            console.log('Event Keys:', Object.keys(data.event));
        }
        
        if (data.type) {
            console.log('Struktur: data.type gefunden');
            console.log('Direct Type:', data.type);
        }
        
        // Versuche verschiedene Event-Strukturen
        let eventType: string;
        let eventData: any;
        
        if (data.event && data.event.type) {
            eventType = data.event.type;
            eventData = data.event;
        } else if (data.type) {
            eventType = data.type;
            eventData = data;
        } else {
            console.log('UNBEKANNTE EVENT-STRUKTUR:', data);
            // Logge alle Properties
            for (const key in data) {
                console.log(`Property ${key}:`, data[key]);
            }
            return;
        }
        
        console.log('=== EVENT VERARBEITUNG ===');
        console.log('Event Type:', eventType);
        console.log('Event Data:', eventData);
        
        switch (eventType) {
            case 'connection':
                console.log('✅ CONNECTION EVENT:', eventData.message || 'Verbindung hergestellt');
                break;
                
            case 'heartbeat':
                console.log('💓 HEARTBEAT EVENT, Timestamp:', eventData.timestamp);
                break;
                
            case 'item-change':
                console.log('🔄 ITEM-CHANGE EVENT für:', eventData.item?.name);
                if (eventData.item) {
                    console.log('Item Details:', eventData.item);
                    this.itemGroupComponents.forEach(component => {
                        component.updateItem(eventData.item);
                    });
                    console.log('✅ Item Update an UI-Komponenten gesendet');
                } else {
                    console.error('❌ Item-Change Event ohne Item-Daten:', eventData);
                }
                break;
                
            default:
                console.log('❓ UNBEKANNTER EVENT TYPE:', eventType, 'Data:', eventData);
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
