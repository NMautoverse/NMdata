$(document).ready(function() {

  $chunks = $('.fold');

  $chunks.each(function () {

    // add button to source code chunks
    if ( $(this).hasClass('s') ) {
      $(this).prepend("<div class=\"showopt\">Show Source</div><br style=\"line-height:22px;\"/>");
      $('pre.r', this).addClass('foldable');

    }

    // add button to output chunks
    if ( $(this).hasClass('o') ) {
      $(this).prepend("<div class=\"showopt\">Show Output</div><br style=\"line-height:22px;\"/>");
      $('pre:not(.r)', this).addClass('foldable');

      // add button to plots
      $(this).find('img').wrap('<pre class=\"plot\"></pre>');
      $('pre.plot', this).children('img').addClass('foldable');

    }
  });

  // hide all chunks when document is loaded
  $('.foldable').css('display', 'none')
	
  // function to toggle the visibility
  $('.showopt').click(function() {
    var label = $(this).html();
    if (label.indexOf("Show") >= 0) {
      $(this).html(label.replace("Show", "Hide"));
    } else {
      $(this).html(label.replace("Hide", "Show"));
    }
    $(this).parent().find(".foldable").slideToggle('fast', 'swing');
  });
});