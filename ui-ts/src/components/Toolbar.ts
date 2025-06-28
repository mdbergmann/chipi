export class Toolbar {
    private element: HTMLElement;
    private onApiKeyClick: () => void;

    constructor(container: HTMLElement, onApiKeyClick: () => void) {
        this.onApiKeyClick = onApiKeyClick;
        this.element = this.createElement();
        container.appendChild(this.element);
    }

    private createElement(): HTMLElement {
        const toolbar = document.createElement('div');
        toolbar.className = 'toolbar';
        
        const title = document.createElement('h1');
        title.textContent = 'Chipi UI';
        
        const apiKeyButton = document.createElement('button');
        apiKeyButton.textContent = 'API-Schlüssel';
        apiKeyButton.addEventListener('click', this.onApiKeyClick);
        
        toolbar.appendChild(title);
        toolbar.appendChild(apiKeyButton);
        
        return toolbar;
    }
}
