import { getApiKey } from './storage';

export interface SSEItemChangeEvent {
    type: 'item-change';
    data: {
        name: string;
        label: string;
        'type-hint': string;
        tags: Record<string, any>;
        'item-state': {
            value: any;
            timestamp: number;
        };
    };
}

export interface SSEConnectionEvent {
    type: 'connection';
    message: string;
}

export interface SSEHeartbeatEvent {
    type: 'heartbeat';
    timestamp: number;
}

export type SSEEvent = SSEItemChangeEvent | SSEConnectionEvent | SSEHeartbeatEvent;

export class ItemEventSource {
    private eventSource: EventSource | null = null;
    private reconnectTimeout: number | null = null;
    private reconnectDelay = 1000; // Start with 1 second
    private maxReconnectDelay = 30000; // Max 30 seconds
    private isConnecting = false;
    private shouldReconnect = true;

    constructor(
        private onItemChange: (event: SSEItemChangeEvent) => void,
        private onConnection?: (event: SSEConnectionEvent) => void,
        private onError?: (error: Event) => void
    ) {}

    connect(): void {
        if (this.isConnecting || this.eventSource) {
            return;
        }

        this.isConnecting = true;
        this.shouldReconnect = true;

        try {
            const apiKey = getApiKey();
            if (!apiKey) {
                console.error('No API key available for SSE connection');
                this.isConnecting = false;
                return;
            }

            const url = `/events/items?apikey=${encodeURIComponent(apiKey)}`;
            const fullUrl = new URL(url, window.location.origin).toString();
            console.log('Attempting to connect to SSE endpoint:', fullUrl);
            
            this.eventSource = new EventSource(url);

            // Add this debugging
            console.log('Creating EventSource for /events/items');
            console.log('EventSource created, readyState:', this.eventSource.readyState);

            this.eventSource.onopen = () => {
                console.log('SSE connection opened, readyState:', this.eventSource?.readyState);
                this.isConnecting = false;
                this.reconnectDelay = 1000; // Reset reconnect delay on successful connection
            };

            this.eventSource.onmessage = (event) => {
                try {
                    const data: SSEEvent = JSON.parse(event.data);
                    this.handleSSEEvent(data);
                } catch (error) {
                    console.error('Error parsing SSE event:', error);
                }
            };

            this.eventSource.onerror = (error) => {
                console.error('SSE connection error:', error);
                this.isConnecting = false;
                
                if (this.onError) {
                    this.onError(error);
                }

                // Handle reconnection
                if (this.shouldReconnect) {
                    this.scheduleReconnect();
                }
            };

        } catch (error) {
            console.error('Failed to create EventSource:', error);
            this.isConnecting = false;
            if (this.shouldReconnect) {
                this.scheduleReconnect();
            }
        }
    }

    private handleSSEEvent(event: SSEEvent): void {
        switch (event.type) {
            case 'item-change':
                this.onItemChange(event);
                break;
            case 'connection':
                console.log('SSE connection established:', event.message);
                if (this.onConnection) {
                    this.onConnection(event);
                }
                break;
            case 'heartbeat':
                console.debug('SSE heartbeat received');
                break;
            default:
                console.warn('Unknown SSE event type:', event);
        }
    }

    private scheduleReconnect(): void {
        if (this.reconnectTimeout) {
            clearTimeout(this.reconnectTimeout);
        }

        console.log(`Reconnecting in ${this.reconnectDelay}ms...`);
        
        this.reconnectTimeout = window.setTimeout(() => {
            this.disconnect();
            this.connect();
            
            // Exponential backoff with jitter
            this.reconnectDelay = Math.min(
                this.reconnectDelay * 2 + Math.random() * 1000,
                this.maxReconnectDelay
            );
        }, this.reconnectDelay);
    }

    disconnect(): void {
        this.shouldReconnect = false;
        
        if (this.reconnectTimeout) {
            clearTimeout(this.reconnectTimeout);
            this.reconnectTimeout = null;
        }

        if (this.eventSource) {
            this.eventSource.close();
            this.eventSource = null;
        }

        this.isConnecting = false;
    }

    isConnected(): boolean {
        return this.eventSource?.readyState === EventSource.OPEN;
    }

    getReadyState(): number {
        return this.eventSource?.readyState ?? EventSource.CLOSED;
    }
}

// Singleton instance for global use
let globalEventSource: ItemEventSource | null = null;

export function getGlobalEventSource(
    onItemChange: (event: SSEItemChangeEvent) => void,
    onConnection?: (event: SSEConnectionEvent) => void,
    onError?: (error: Event) => void
): ItemEventSource {
    if (!globalEventSource) {
        globalEventSource = new ItemEventSource(onItemChange, onConnection, onError);
    }
    return globalEventSource;
}

export function disconnectGlobalEventSource(): void {
    if (globalEventSource) {
        globalEventSource.disconnect();
        globalEventSource = null;
    }
}
