// Add active class to current nav link
const setActiveLink = () => {
    const currentPath = window.location.pathname;
    document.querySelectorAll('nav a').forEach(link => {
        const linkPath = link.getAttribute('href');
        link.classList.toggle('active', currentPath.endsWith(linkPath));
    });
};

// Initialize router
document.addEventListener('DOMContentLoaded', () => {
    setActiveLink();
});

// Update active link on navigation
window.addEventListener('popstate', setActiveLink);
