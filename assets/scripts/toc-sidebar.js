/**
 * Sticky Floating Table of Contents Sidebar
 *
 * Features:
 * - Automatically creates a sticky sidebar from the existing TOC
 * - Highlights current section based on scroll position
 * - Smooth scrolling to sections
 * - Responsive with toggle button on mobile/tablet
 * - Collapsible nested sections
 */

(function() {
  'use strict';

  // Configuration
  const CONFIG = {
    // Minimum screen width to always show the sidebar (in pixels)
    minWidth: 1280,
    // Offset from top when highlighting sections (accounts for any fixed headers)
    scrollOffset: 100,
    // Debounce delay for scroll events (in milliseconds)
    scrollDebounce: 50
  };

  /**
   * Initialize the sticky TOC sidebar
   */
  function initStickyTOC() {
    const originalTOC = document.getElementById('table-of-contents');

    // Only proceed if there's a TOC
    if (!originalTOC) {
      return;
    }

    // Create the sidebar
    const sidebar = createSidebar(originalTOC);
    document.body.appendChild(sidebar);

    // Create toggle button (for mobile/tablet)
    const toggleButton = createToggleButton(sidebar);
    document.body.appendChild(toggleButton);

    // Set up scroll spy
    setupScrollSpy(sidebar);

    // Handle window resize
    let resizeTimeout;
    window.addEventListener('resize', () => {
      clearTimeout(resizeTimeout);
      resizeTimeout = setTimeout(() => {
        updateResponsiveState(sidebar, toggleButton);
      }, 150);
    });

    // Initial state
    updateResponsiveState(sidebar, toggleButton);
  }

  /**
   * Update responsive state based on screen size
   */
  function updateResponsiveState(sidebar, toggleButton) {
    if (window.innerWidth >= CONFIG.minWidth) {
      // Desktop: always show sidebar, hide toggle
      sidebar.classList.remove('mobile');
      sidebar.classList.add('visible');
      toggleButton.style.display = 'none';
    } else {
      // Mobile/tablet: hide sidebar by default, show toggle
      sidebar.classList.add('mobile');
      sidebar.classList.remove('visible');
      toggleButton.style.display = 'flex';
    }
  }

  /**
   * Create toggle button for mobile
   */
  function createToggleButton(sidebar) {
    const button = document.createElement('button');
    button.id = 'toc-toggle';
    button.className = 'toc-toggle';
    button.setAttribute('aria-label', 'Toggle table of contents');
    button.innerHTML = `
      <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
        <line x1="3" y1="12" x2="21" y2="12"></line>
        <line x1="3" y1="6" x2="21" y2="6"></line>
        <line x1="3" y1="18" x2="21" y2="18"></line>
      </svg>
    `;

    button.addEventListener('click', () => {
      sidebar.classList.toggle('visible');
      button.classList.toggle('active');
    });

    // Close sidebar when clicking outside on mobile
    document.addEventListener('click', (e) => {
      if (window.innerWidth < CONFIG.minWidth &&
          sidebar.classList.contains('visible') &&
          !sidebar.contains(e.target) &&
          !button.contains(e.target)) {
        sidebar.classList.remove('visible');
        button.classList.remove('active');
      }
    });

    // Close sidebar when clicking a link on mobile
    const links = sidebar.querySelectorAll('a[href^="#"]');
    links.forEach(link => {
      link.addEventListener('click', () => {
        if (window.innerWidth < CONFIG.minWidth) {
          sidebar.classList.remove('visible');
          button.classList.remove('active');
        }
      });
    });

    return button;
  }

  /**
   * Create the sidebar element
   */
  function createSidebar(originalTOC) {
    const sidebar = document.createElement('div');
    sidebar.id = 'toc-sidebar';
    sidebar.className = 'toc-sidebar';

    // Clone the TOC content
    const tocContent = originalTOC.querySelector('#text-table-of-contents');
    if (tocContent) {
      const clonedContent = tocContent.cloneNode(true);

      // Add heading
      const heading = document.createElement('h2');
      heading.textContent = 'Contents';
      heading.className = 'toc-sidebar-heading';

      sidebar.appendChild(heading);
      sidebar.appendChild(clonedContent);

      // Enhance links for smooth scrolling
      enhanceLinks(sidebar);
    }

    return sidebar;
  }

  /**
   * Enhance TOC links with smooth scrolling
   */
  function enhanceLinks(sidebar) {
    const links = sidebar.querySelectorAll('a[href^="#"]');
    links.forEach(link => {
      link.addEventListener('click', (e) => {
        e.preventDefault();
        const targetId = link.getAttribute('href').slice(1);
        const target = document.getElementById(targetId);

        if (target) {
          const offset = CONFIG.scrollOffset;
          const targetPosition = target.getBoundingClientRect().top + window.pageYOffset - offset;

          window.scrollTo({
            top: targetPosition,
            behavior: 'smooth'
          });

          // Update active link immediately
          updateActiveLink(link);
        }
      });
    });
  }

  /**
   * Set up scroll spy to highlight current section
   */
  function setupScrollSpy(sidebar) {
    const links = Array.from(sidebar.querySelectorAll('a[href^="#"]'));
    const sections = links.map(link => {
      const id = link.getAttribute('href').slice(1);
      return document.getElementById(id);
    }).filter(section => section !== null);

    let scrollTimeout;
    let ticking = false;

    function onScroll() {
      if (!ticking) {
        window.requestAnimationFrame(() => {
          updateActiveSection(sections, links);
          ticking = false;
        });
        ticking = true;
      }
    }

    window.addEventListener('scroll', onScroll, { passive: true });

    // Initial update
    updateActiveSection(sections, links);
  }

  /**
   * Update which section is marked as active
   */
  function updateActiveSection(sections, links) {
    const scrollPosition = window.pageYOffset + CONFIG.scrollOffset;

    // Find the current section
    let currentSectionIndex = -1;
    for (let i = sections.length - 1; i >= 0; i--) {
      const section = sections[i];
      if (section.offsetTop <= scrollPosition) {
        currentSectionIndex = i;
        break;
      }
    }

    // Update active link
    links.forEach((link, index) => {
      if (index === currentSectionIndex) {
        link.classList.add('active');
      } else {
        link.classList.remove('active');
      }
    });
  }

  /**
   * Update active link directly (for click events)
   */
  function updateActiveLink(clickedLink) {
    const sidebar = document.getElementById('toc-sidebar');
    if (!sidebar) return;

    const links = sidebar.querySelectorAll('a[href^="#"]');
    links.forEach(link => {
      if (link === clickedLink) {
        link.classList.add('active');
      } else {
        link.classList.remove('active');
      }
    });
  }

  // Initialize when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initStickyTOC);
  } else {
    initStickyTOC();
  }
})();
