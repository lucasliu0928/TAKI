$(document).ready(function(){
	$('.each_tag').on('click', function(){
		if($(this).prev().is(':checked')){
			$(this).css({background: "#fff", color: "#000"});
		}else{
			$(this).css({background: "#89C4F4", color: "#fff"});
		}
		$(this).prev().click();
	});
	$('.search_icon').click(function(){
    $('#search_on_site').focus();
	});

	$('.dropdown-in-menu').click(function(){
		if($(this).attr('class').indexOf('openned')!= -1){
	    $(this).children('ul').slideUp();
	    $(this).removeClass('openned');
		}else{
	    $(this).children('ul').slideDown();
	    $(this).addClass('openned');
		}
	});

});
