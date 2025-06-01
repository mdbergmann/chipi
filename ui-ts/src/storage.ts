const KEY = 'chipi-api-key';

export const getApiKey = (): string | null =>
  localStorage.getItem(KEY);

export const setApiKey = (key: string): void =>
  localStorage.setItem(KEY, key);

export const clearApiKey = (): void =>
  localStorage.removeItem(KEY);
