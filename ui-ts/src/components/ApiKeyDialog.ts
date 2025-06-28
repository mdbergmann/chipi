import { saveApiKey } from '../utils/storage';

export class ApiKeyDialog {
    private element: HTMLElement;
    private onApiKeySet: (apiKey: string) => void;

    constructor(onApiKeySet: (apiKey: string) => void) {
        this.onApiKeySet = onApiKeySet;
        this.element = this.createElement();
        document.body.appendChild(this.element);
    }

    private createElement(): HTMLElement {
        const modal = document.getElementById('apiKeyModal')!;
        const input = document.getElementById('apiKeyInput') as HTMLInputElement;
        const saveButton = document.getElementById('saveApiKey')!;
        const cancelButton = document.getElementById('cancelApiKey')!;

        saveButton.addEventListener('click', () => {
            const apiKey = input.value.trim();
            if (apiKey) {
                saveApiKey(apiKey);
                this.onApiKeySet(apiKey);
                this.hide();
                input.value = '';
            }
        });

        cancelButton.addEventListener('click', () => {
            this.hide();
            input.value = '';
        });

        // Close on Escape key
        document.addEventListener('keydown', (e) => {
            if (e.key === 'Escape' && !modal.classList.contains('hidden')) {
                this.hide();
                input.value = '';
            }
        });

        return modal;
    }

    show(): void {
        this.element.classList.remove('hidden');
        const input = document.getElementById('apiKeyInput') as HTMLInputElement;
        input.focus();
    }

    hide(): void {
        this.element.classList.add('hidden');
    }
}
