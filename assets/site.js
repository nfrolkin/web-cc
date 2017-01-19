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

function search(searchString, parentElement) {
  var cleanedSearchString, elements;
  cleanedSearchString = searchString.toLowerCase();
  elements = parentElement.querySelectorAll(".searched");

  elements.forEach(function(item, i, els) {
    var text = item.innerHTML.toLowerCase();
    if (text.search(cleanedSearchString) > -1) {
      item.style.display = "";
    } else {
      item.style.display = "none";
    }
  });
}
