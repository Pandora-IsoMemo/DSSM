$(function(){
  $(".clipboard button").on("click", function(){
    $("textarea", $(this).parent()).select();
    var success = document.execCommand("copy");

    if (success){
      alert("Text has been copied to the clipboard");
    } else {
      alert("Text could not be copied to the clipboard");
    }
  })
})
