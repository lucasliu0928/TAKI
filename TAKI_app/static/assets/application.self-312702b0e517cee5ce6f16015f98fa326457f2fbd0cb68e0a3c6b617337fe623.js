// This is a manifest file that'll be compiled into application.js, which will include all the files
// listed below.
//
// Any JavaScript/Coffee file within this directory, lib/assets/javascripts, vendor/assets/javascripts,
// or any plugin's vendor/assets/javascripts directory can be referenced here using a relative path.
//
// It's not advisable to add code directly here, but if you do, it'll appear at the bottom of the
// compiled file. JavaScript code in this file should be added after the last require_* statement.
//
// Read Sprockets README (https://github.com/rails/sprockets#sprockets-directives) for details
// about supported directives.
//
// # require jquery.form





















$(document).on('change', '#avatar', function(e) {
	var options = {}
    e.preventDefault(); // prevent native submit
    $('#people_form').ajaxSubmit(options);
});

$(document).on('focus', '.login-field', function(){
	$(this).prev().css("margin-top", -30);
});

$(document).on('click', '.btn-link', function(){
  if ($(this).prev().attr('class').indexOf('fa-plus') != -1) {
    $(this).prev().removeClass('fa-plus');
    $(this).prev().addClass('fa-minus');
  } else {
    $(this).prev().addClass('fa-plus');
    $(this).prev().removeClass('fa-minus');
  }
    // var dataTarget=$(this).attr('data-target').replace(/^#/,'' );
    // var dTelem=document.getElementById(dataTarget);
    // if ( ! $(dTelem).undefined ) {
    //     $(dTelem).collapse("show");
    // }
});
$(document).on('click', '.event-expand', function() {
  if ($(this).attr('class').indexOf('fa-plus') != -1) {
    $(this).removeClass('fa-plus');
    $(this).addClass('fa-minus');
  } else {
    $(this).addClass('fa-plus');
    $(this).removeClass('fa-minus');
  }
});


// $(document).on('mouseleave', '.collapsable-content', function(){
//     var expandedState = $(this).attr('aria-expanded');
//     if ( expandedState ) {
//         $(this).collapse("hide");
//     }
// });
$(document).on('blur', '.login-field', function(){
	if($(this).val() == ''){
		$(this).prev().css("margin-top", 0);
	}
});

$(document).on('keyup', '.login-field', function(){
  if($(this).val() == ''){
  	$(this).prev().css("margin-top", 0);
  }else{
  	$(this).prev().css("margin-top", -30);
  }
});
$(document).ready(function(){
	$('.login-field').each(function(){
    if($(this).attr('class').indexOf('password-field') != -1){
	  	$(this).prev().css("margin-top", -30);
    }else{
      if($(this).val() != ''){
        $(this).prev().css("margin-top", -30);
  	  }
    }
	});
});

$(document).on('click', '.add-event',function(){
  $(this).prev().click();
});

$(document).on('click', '.remove_item',function(){
  $(this).parent().next().next().click();
});

$(document).on('change', '.attach-image', function(e) {
	var options = {}
    e.preventDefault(); // prevent native submit
    $('.form-with-image').ajaxSubmit(options);
});

$(document).on('click', '.remove_news', function(){
	if (confirm("Are you sure you want to delete this news?")) {
        // deletion code
	  $('#delete_news').val($(this).attr('id'));
	  $('#delete_news').click();
  }
});

// $(document).on('click', '.edit_news', function(){
// 	$('#list-left, #list-right').sortable({
// 	  disabled: true
// 	});
//   $(this).parent().parent().parent().prev().show();
//   $(this).parent().parent().parent().hide();
// });
//



// $(document).on('click', '.editable', function(){
// 	if($(this).prop('tagName') == "TEXTAREA"){
// 	  $(this).jqte({fsizes: ["10", "15", "20", "25", "30", "32", "36"]});
// 	  $(this).parent().prev().css('min-height', 400);
// 	}else{
//     $(this).jqte({fsizes: ["10", "15", "20", "25", "30", "32", "36"]});
// 	  $(this).parent().prev().css('min-height', 20);
// 	}
// });
//

$(document).on('change', '.menu_icon', function(){
	var menu_id = $(this).attr('menu_id');
	var value = $(this).val();
	$(this).next().next().attr('class', 'fa fa-'+$(this).val()+' nice-icon');
	$.ajax({
		url: '/menus/'+menu_id,
		data: {menu: {icon: value}},
		dataType: 'SCRIPT',
		type: 'PUT'
	})

});

$(document).on('focusout', '.menu_name', function(){
	var menu_id = $(this).attr('menu_id');
	var value = $(this).val();
	$.ajax({
		url: '/menus/'+menu_id,
		data: {menu: {name: value}},
		dataType: 'SCRIPT',
		type: 'PUT'
	})
});

$(document).on('change', '.menu_url', function(){
	var menu_id = $(this).attr('menu_id');
	var value = $(this).val();
	$.ajax({
		url: '/menus/'+menu_id,
		data: {menu: {url: value}},
		dataType: 'SCRIPT',
		type: 'PUT'
	})
});

function getcontact(contactelem) {
    var content = contactelem.innerHTML;
    var c1      = content.split('').reverse().join('');
    var dir     = contactelem.style.direction;
    if ( dir.length == 0 ) {
        var newelem = document.createTextNode(c1);
        contactelem.replaceChild(newelem,contactelem.childNodes[0]);
        contactelem.style.direction="ltr";
    }
}

// Append an item to the <ul> element within the parent
// of the parent of the selected item
$(document).on("click","button.add-site-btn" , function(){
//    console.log( "add_site_button pressed" );
    var this_parent       = $(this).parent();
    var ul                = $(this).parent().prev();
    var website_text_elem = $(this).prev();
    var website_url_elem  = website_text_elem.prev();
    var url               = website_url_elem[0].value;
    var name              = website_text_elem[0].value;

    if ( name.length > 0 && url.length > 0 ) {
        var aElem     = document.createElement("a");
        var liElem    = document.createElement("li");
        var itemLiDelete = document.createElement("button");
        aElem.innerText = name;
        aElem.setAttribute("href",url);
        itemLiDelete.setAttribute("type","button");
        itemLiDelete.classList.add("ibi-btn","delete-list-item");
        itemLiDelete.innerHTML = '&nbsp;Remove&nbsp;';
        liElem.classList.add("item-site");
        liElem.append( aElem, itemLiDelete );
        ul.append( liElem );
        website_url_elem.value = "";
        website_text_elem.value = "";
        website_url_elem.focus();
//        console.log( "add_site_button a element created" );
    }
})


// Append an item to the <ul> element within the parent
// of the parent of the selected item
$(document).on("click","button.add-item-btn" , function(){
    console.log( "add_item_button pressed" );
    var this_parent = $(this).parent();
    var ul       = $(this).parent().prev();
    var credElem = $(this).prev();
    var cred     = credElem.val();

    if ( cred.length > 0 ) {
        var credTextElem = document.createTextNode( cred );
        var credLiElem   = document.createElement("li");
        var credLiDelete = document.createElement("button");
        credLiDelete.setAttribute("type","button");
        credLiDelete.classList.add("ibi-btn","delete-list-item");
        credLiDelete.innerHTML = '&nbsp;Remove&nbsp;';
        credLiElem.classList.add("item-text")
        credLiElem.append( credTextElem, credLiDelete );
        ul.append( credLiElem );
        credElem[0].value = "";
        credElem[0].focus();
    }
})

$(document).on("click","button.delete-list-item" , function(){
    console.log("delete-list-item button pressed" );
    var this_parent = $(this).parent();
    console.log( "delete-list-item: parent   => "+this_parent.tagName);
    this_parent.remove();
})

$(document).on("click",".ibi-submit-form",function(e){
    var text_fields = $("form").find(".text-item-submit-value");
    var site_fields = $("form").find(".site-item-submit-value");

//    alert("submit: ibi-submit-form" );
    
    console.log( "submit-ibi-form: found "+
                 text_fields.length+
                 " hidden values, array? " + Array.isArray( text_fields));
    $(text_fields).each( function( idx0, text_field ) {
        var value_array = [];
        var ul          = text_field.nextElementSibling;
        var li_s        = $(ul).find("li.item-text");
        $(li_s).each( function( idx1, li ) {
            var li_value = li.childNodes[0];
            //           debugger;
            if ( ! li_value.undefined ) {
                var value_content = li_value.wholeText.replace(/\s*$/,"");
                if ( !value_content.undefined &&  value_content.length > 0 ) {
                    value_array.push(value_content);
                    console.log( "text_field."+text_field.name+": "+text_field.value );
                }
            }
        })
        var value_text = JSON.stringify( value_array );
        text_field.value = value_text;
    })
//    alert("submit: "+text_fields.value);
    $(site_fields).each( function( idx2, site_field ) {
        var value_array = [];
        var ul          = site_field.nextElementSibling;
        var li_s        = $(ul).find("li.item-site");
        $(li_s).each( function( idx3, li ) {
            var li_a = li.childNodes[0];
            //           debugger;
            if ( ! li_a.undefined ) {
                var value_href    = li_a.getAttribute("href");
                var value_content = li_a.innerText.replace(/\s*$/,"");
                if ( !value_content.undefined &&  value_content.length > 0 &&
                     !value_href.undefined    &&  value_href.length > 0 ) {
                    var site = {website_url:value_href,website_displayText:value_content};
                    value_array.push(site);
                    console.log( "site: "+site );
                }
            }
        })
        var value_text = JSON.stringify( value_array );
        site_field.value = value_text;
    })    
//    alert("submited: "+site_fields.value);
//    console.log( "submit-ibi-form: complete");
//   debugger;
})

$(document).ready(function(){
  // var isFirefox = typeof InstallTrigger !== 'undefined';
  // if (!isFirefox) {
    $('#news_published').datepicker();
  // }
  // sortable in editing
  $( ".sortable" ).sortable();
  $( ".sortable" ).disableSelection();
});

function author_color(num){
	switch(num%12){
    case 0:
      return "#1abc9c"
      break;
    case 1:
      return "#2ecc71"
      break;
    case 2:
      return "#3498db"
      break;
    case 3:
      return "#9b59b6"    
      break;
    case 4:
      return "#34495e"
      break;
    case 5:
      return "#f39c12"
      break;
    case 6:
      return "#e74c3c"
      break;
    case 7:
      return "#e67e22"
      break;
    case 8:
      return "#8e44ad"
      break;
    case 9:
      return "#2980b9"
      break;
    case 10:
      return "#27ae60"
      break;
    case 11:
      return "#16a085"
      break;
  }
}

// reset forms
function resetForm(formStr) {
  $('#' + formStr + ' select, #' + formStr + ' input').not('[type="hidden"]').each(function(){
    $(this).val("");
  });
  for (var i = 0; i < tinymce.editors.length; i++) {
    var editorInstance = tinymce.editors[i];
    editorInstance.setContent('');
  }
}
function resetPublicationForm() {
  $('#identified-authors').html("");
  $('#publication-form select, #publication-form input').not('[type="hidden"]').each(function(){
    if ($(this).attr('id') !== "publication_url") {
      $(this).val("");
    }
    if ($(this).next().attr('class') === "message-text") {
      resetMessage($(this));
    }
  });
}

$(document).on('focusin', '.news-content', function(){
    tinymce.init({
        height: 100,
    });
});

// homepage
$(document).ready(function(){
  $('#home-image-text').height($('#home-image').height());
});

$( window ).resize(function() {
  if ($(window).width() < 991) {
    $('#home-image-text').height(200);
  } else{
    $('#home-image-text').height($('#home-image').height());
  }
});

$(document).on('click', '#news-events-goto', function() {
  var news_events_div = document.getElementById('new-events-div');
  var height = news_events_div.offsetTop;
  window.scroll(0, height - 122);
});
