import { Item, ItemGroup, ApiError, SSEEvent, UpdateItemRequest } from './types';

export class ChipiApiClient {
    private baseUrl: string;
    private apiKey: string | null = null;

    constructor(baseUrl: string = '') {
        this.baseUrl = baseUrl;
    }

    setApiKey(apiKey: string) {
        this.apiKey = apiKey;
    }

    private getHeaders(): HeadersInit {
        const headers: HeadersInit = {
            'Content-Type': 'application/json',
        };
        
        if (this.apiKey) {
            headers['X-Api-Key'] = this.apiKey;
        }
        
        return headers;
    }

    private async handleResponse<T>(response: Response): Promise<T> {
        if (!response.ok) {
            if (response.status === 401) {
                throw new Error('Nicht autorisiert - API-Schlüssel fehlt oder ungültig');
            }
            if (response.status === 403) {
                throw new Error('Zugriff verweigert - unzureichende Berechtigung');
            }
            
            try {
                const error: ApiError = await response.json();
                throw new Error(error.message || error.error || `HTTP ${response.status}`);
            } catch {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }
        }
        
        return response.json();
    }

    async getItems(): Promise<Item[]> {
        const response = await fetch(`${this.baseUrl}/items`, {
            headers: this.getHeaders(),
        });
        return this.handleResponse<Item[]>(response);
    }

    async getItem(name: string): Promise<Item> {
        const response = await fetch(`${this.baseUrl}/items/${encodeURIComponent(name)}`, {
            headers: this.getHeaders(),
        });
        return this.handleResponse<Item>(response);
    }

    async updateItem(name: string, value: any): Promise<void> {
        const body: UpdateItemRequest = { value };
        const response = await fetch(`${this.baseUrl}/items/${encodeURIComponent(name)}`, {
            method: 'POST',
            headers: this.getHeaders(),
            body: JSON.stringify(body),
        });
        await this.handleResponse<void>(response);
    }

    async getItemGroups(): Promise<ItemGroup[]> {
        const response = await fetch(`${this.baseUrl}/itemgroups`, {
            headers: this.getHeaders(),
        });
        return this.handleResponse<ItemGroup[]>(response);
    }

    async getItemGroup(name: string): Promise<ItemGroup> {
        const response = await fetch(`${this.baseUrl}/itemgroups/${encodeURIComponent(name)}`, {
            headers: this.getHeaders(),
        });
        return this.handleResponse<ItemGroup>(response);
    }

    createEventSource(): EventSource | null {
        if (!this.apiKey) {
            return null;
        }
        
        // Für Entwicklung: Proxy verwenden (leerer baseUrl bedeutet relative URL)
        const url = `/events/items?apikey=${encodeURIComponent(this.apiKey)}`;
        console.log('Creating EventSource with URL:', url); // Debug-Log
        return new EventSource(url);
    }
}
