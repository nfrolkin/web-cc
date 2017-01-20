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

function focusSearch(element) {
  element.parentElement.querySelector(".search-focus").focus();
}

function search(searchString, parentElement, selector) {
  var cleanedSearchString, elements;
  cleanedSearchString = searchString.toLowerCase();
  elements = parentElement.querySelectorAll(".searched");

  elements.forEach(function(item, i, els) {
    var text;

    if (typeof selector === 'undefined') {
      text = item.innerHTML.toLowerCase();
    } else {
      text = item.querySelector(selector).innerHTML.toLowerCase();
    }
    if (text.search(cleanedSearchString) > -1) {
      item.style.display = "";
    } else {
      item.style.display = "none";
    }
  });
}

function insert(string, elementIdTo) {
  var elementTo;
  elementTo = document.getElementById(elementIdTo);

  elementTo.value += string;
  elementTo.focus();
}
