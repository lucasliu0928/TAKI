(function() {
  $(document).on("click", ".edit-field-btn", function() {
    $(this).parent().parent().next().show();
    return $(this).parent().parent().hide();
  });

}).call(this);
