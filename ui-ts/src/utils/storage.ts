const API_KEY_STORAGE_KEY = 'chipi-api-key';

export function saveApiKey(apiKey: string): void {
    localStorage.setItem(API_KEY_STORAGE_KEY, apiKey);
}

export function loadApiKey(): string | null {
    return localStorage.getItem(API_KEY_STORAGE_KEY);
}

export function removeApiKey(): void {
    localStorage.removeItem(API_KEY_STORAGE_KEY);
}
