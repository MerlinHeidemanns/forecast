const setActiveLink = () => {
    const currentPath = window.location.pathname;
    // Remove trailing slash if present for consistency
    const normalizedPath = currentPath.replace(/\/$/, '');

    document.querySelectorAll('nav a').forEach(link => {
        const linkPath = link.getAttribute('href');
        // Exact match comparison
        const isActive = normalizedPath === linkPath ||
            (linkPath === '/' && normalizedPath === '');
        link.classList.toggle('active', isActive);
    });
};

// Initialize router when DOM is ready
document.addEventListener('DOMContentLoaded', () => {
    setActiveLink();
});

// Update active link on navigation
window.addEventListener('popstate', setActiveLink);