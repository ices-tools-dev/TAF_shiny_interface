callback <- c(
  "$('input[name=rdbtn]').on('click', function(){",
  "  var value = $('input[name=rdbtn]:checked').val();",
  "  Shiny.setInputValue('rdbtn', value);",
  "});"
)