$(document).ready(function() {
  Shiny.addCustomMessageHandler('controlSpinner', function(message) {
    var spinner = $("#loadingSpinnerCountry"); // Your spinner ID
    if (message.action === "show") {
      spinner.find("h3").text(message.message); // Update text
      spinner.show();
    } else if (message.action === "hide") {
      spinner.hide();
    }
  });
});