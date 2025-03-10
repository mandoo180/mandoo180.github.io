window.onload = function() {
  const $toc = document.querySelector('#table-of-contents');
  const $wrapper = document.createElement('div');
  $wrapper.classList.add('toc-toggle-button');
  const $button = document.createElement('button');
  $button.innerText = 'TOC';
  $button.addEventListener('click', function() {
    $toc.classList.toggle('shown');
  });
  $wrapper.appendChild($button);
  $toc.appendChild($wrapper);
}
