// add onClick attribute to every menu item
function addMenuOnClickEvents() {
  $("div.sidebar a.nav-link").attr("onClick", "onMenuSelect(this.id.substr(4))"); // bs4Dash prepends 'tab-' to the menuItem name given in R
}

// send event to R, will be available as input$menuItem
function onMenuSelect(id) {
  Shiny.setInputValue("menuItem", {id:id}, {priority: "event"});
}

addMenuOnClickEvents()