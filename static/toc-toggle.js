function toggleToc() {
  const content = document.getElementById("text-table-of-contents");
  const icon = document.querySelector(".toggle-icon");

  if (content && icon) {
    content.classList.toggle("expanded");
    if (content.classList.contains("expanded")) {
      icon.textContent = "-";
    } else {
      icon.textContent = "+";
    }
  }
}

document.addEventListener("DOMContentLoaded", function () {
  const content = document.getElementById("text-table-of-contents");
  const icon = document.querySelector(".toggle-icon");

  if (content && icon) {
    content.classList.remove("expanded");
  }
});
