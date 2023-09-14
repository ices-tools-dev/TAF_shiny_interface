function detect_click(el) {
    Shiny.onInputChange('clicked_text', el.innerHTML);
  }
  
  