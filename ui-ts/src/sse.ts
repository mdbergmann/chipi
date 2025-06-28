import { getApiKey } from './storage';

export interface SSEItemChangeEvent {
    type: 'item-change';
    item: {
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
    private heartbeatTimeout: number | null = null;
    private reconnectDelay = 1000; // Start with 1 second
    private maxReconnectDelay = 30000; // Max 30 seconds
    private isConnecting = false;
    private shouldReconnect = true;
    private heartbeatInterval = 60000; // 60 seconds (server sends every 30s, so 2x + buffer)
    private isOnline = false;

    constructor(
        private onItemChange: (event: SSEItemChangeEvent) => void,
        private onConnection?: (event: SSEConnectionEvent) => void,
        private onError?: (error: Event) => void,
        private onConnectionStateChange?: (connected: boolean) => void
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
                this.isOnline = true;
                this.reconnectDelay = 1000; // Reset reconnect delay on successful connection
                
                // Notify about connection state change
                if (this.onConnectionStateChange) {
                    this.onConnectionStateChange(true);
                }
                // Don't start heartbeat monitoring immediately - wait for first heartbeat or connection event
            };

            this.eventSource.onmessage = (event) => {
                try {
                    console.log('Raw SSE message received:', event.data);
                    
                    // Parse the new JSON format: {"event": {"type": "...", ...}}
                    const wrapper = JSON.parse(event.data);
                    
                    if (!wrapper.event) {
                        console.error('SSE message missing event property:', wrapper);
                        return;
                    }
                    
                    const data: SSEEvent = wrapper.event;
                    console.log('Parsed SSE event:', data);
                    
                    this.handleSSEEvent(data);
                } catch (error) {
                    console.error('Error parsing SSE event:', error, 'Raw data:', event.data);
                }
            };

            this.eventSource.onerror = (error) => {
                console.error('SSE connection error:', error);
                this.isConnecting = false;
                this.setOffline();
                
                // Check if this is a 401 error by trying to detect it
                // EventSource doesn't give us HTTP status, so we need to check differently
                if (this.eventSource?.readyState === EventSource.CLOSED) {
                    // Try a test request to see if it's an auth issue
                    this.checkAuthStatus().then(isAuthError => {
                        if (isAuthError) {
                            console.log('SSE connection failed due to authentication error');
                            // Trigger need-auth event
                            window.dispatchEvent(new CustomEvent('need-auth'));
                            this.shouldReconnect = false; // Don't reconnect on auth errors
                            return;
                        }
                        
                        // Not an auth error, continue with normal error handling
                        if (this.onError) {
                            this.onError(error);
                        }

                        if (this.shouldReconnect) {
                            this.scheduleReconnect();
                        }
                    });
                } else {
                    if (this.onError) {
                        this.onError(error);
                    }

                    if (this.shouldReconnect) {
                        this.scheduleReconnect();
                    }
                }
            };

        } catch (error) {
            console.error('Failed to create EventSource:', error);
            this.isConnecting = false;
            this.setOffline();
            if (this.shouldReconnect) {
                this.scheduleReconnect();
            }
        }
    }

    private startHeartbeatMonitoring(): void {
        this.clearHeartbeatTimeout();
        this.heartbeatTimeout = window.setTimeout(() => {
            console.warn('Heartbeat timeout - connection appears to be dead');
            this.setOffline();
            if (this.shouldReconnect) {
                this.scheduleReconnect();
            }
        }, this.heartbeatInterval);
    }

    private clearHeartbeatTimeout(): void {
        if (this.heartbeatTimeout) {
            clearTimeout(this.heartbeatTimeout);
            this.heartbeatTimeout = null;
        }
    }

    private setOffline(): void {
        if (this.isOnline) {
            this.isOnline = false;
            if (this.onConnectionStateChange) {
                this.onConnectionStateChange(false);
            }
            if (this.onError) {
                this.onError(new Event('offline'));
            }
        }
        this.clearHeartbeatTimeout();
    }

    private handleSSEEvent(event: SSEEvent): void {
        switch (event.type) {
            case 'item-change':
                this.onItemChange(event);
                this.ensureHeartbeatMonitoring();
                break;
            case 'connection':
                console.log('SSE connection established:', event.message);
                if (this.onConnection) {
                    this.onConnection(event);
                }
                this.ensureHeartbeatMonitoring(); // Start monitoring after connection event
                break;
            case 'heartbeat':
                console.debug('SSE heartbeat received');
                this.ensureHeartbeatMonitoring();
                break;
            default:
                console.warn('Unknown SSE event type:', event);
        }
    }

    private ensureHeartbeatMonitoring(): void {
        this.clearHeartbeatTimeout();
        this.startHeartbeatMonitoring();
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
        this.clearHeartbeatTimeout();
        
        if (this.reconnectTimeout) {
            clearTimeout(this.reconnectTimeout);
            this.reconnectTimeout = null;
        }

        if (this.eventSource) {
            this.eventSource.close();
            this.eventSource = null;
        }

        this.isConnecting = false;
        this.isOnline = false;
    }

    isConnected(): boolean {
        return this.isOnline && 
               this.eventSource?.readyState === EventSource.OPEN && 
               !this.isConnecting;
    }

    getReadyState(): number {
        return this.eventSource?.readyState ?? EventSource.CLOSED;
    }

    private async checkAuthStatus(): Promise<boolean> {
        try {
            const apiKey = getApiKey();
            if (!apiKey) return true; // No API key = auth error
            
            // Make a simple test request to check auth
            const response = await fetch('/itemgroups', {
                headers: {
                    'X-Api-Key': apiKey,
                    'Accept': 'application/json'
                }
            });
            
            return response.status === 401 || response.status === 403;
        } catch (error) {
            // Network error, not auth error
            return false;
        }
    }
}

// Singleton instance for global use
let globalEventSource: ItemEventSource | null = null;

export function getGlobalEventSource(
    onItemChange: (event: SSEItemChangeEvent) => void,
    onConnection?: (event: SSEConnectionEvent) => void,
    onError?: (error: Event) => void,
    onConnectionStateChange?: (connected: boolean) => void
): ItemEventSource {
    if (!globalEventSource) {
        globalEventSource = new ItemEventSource(onItemChange, onConnection, onError, onConnectionStateChange);
    }
    return globalEventSource;
}

export function disconnectGlobalEventSource(): void {
    if (globalEventSource) {
        globalEventSource.disconnect();
        globalEventSource = null;
    }
}
