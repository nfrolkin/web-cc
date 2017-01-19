function openModal(id) {
  var modal = document.getElementById(id);
  modal.style.display = 'block';
  window.onclick = function(event) {
    if (event.target === modal) {
      closeModal(id);
    }
  };
}

function closeModal(id) {
  var modal = document.getElementById(id);
  modal.style.display = 'none';
}

function focusSearch(dropdownElement) {
  dropdownElement.parentElement.querySelector(".dropdown-focus").focus();
}
