$(document).ready(function(){
    $('body').on('click', 'a', function(evt){
        Shiny.setInputValue('clicked_text', evt.target.id);
    });
    })