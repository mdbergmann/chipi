export interface ItemState {
    value: any;
    timestamp: number;
}

export interface Item {
    name: string;
    label: string;
    'type-hint': string;
    tags: Record<string, any>;
    'item-state': ItemState;
}

export interface ItemGroup {
    name: string;
    label: string;
    items: Item[];
}

export interface ApiError {
    error: string;
    message?: string;
}

export interface SSEEvent {
    event: {
        type: 'connection' | 'item-change' | 'heartbeat';
        message?: string;
        item?: Item;
        timestamp?: number;
    };
}

// Alternative Struktur, falls die Events direkt das Item enthalten:
export interface AlternativeSSEEvent {
    type: 'connection' | 'item-change' | 'heartbeat';
    message?: string;
    item?: Item;
    timestamp?: number;
}

// Flexible SSE Event Struktur
export interface FlexibleSSEEvent {
    event?: {
        type: string;
        message?: string;
        item?: Item;
        timestamp?: number;
    };
    type?: string;
    message?: string;
    item?: Item;
    timestamp?: number;
}

export interface UpdateItemRequest {
    value: any;
}
