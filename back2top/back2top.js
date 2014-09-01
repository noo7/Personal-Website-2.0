var Back2top = (function(d,f) {
	var c = 300;
	var b = (function(){
		var g = navigator.userAgent.toLowerCase();
		return {
			version:(g.match(/.+(?:rv|it|ra|ie)[\/: ]([\d.]+)/)||[])[1],msie:/msie/.test(g)&&!/opera/.test(g)
		}
	})();
	var a = function() {
		var g = d('<div id="Back2top">Back to Top</div>').appendTo("body").click(function() {
			d("html, body").animate({scrollTop:0},parseInt(c,10)||300)
		});
		var h = function(){
			var j = d(document).scrollTop(), i = d(window).height();
			(j > 0) ? g.show():g.hide();
			if(b.msie && parseInt(b.version, 10) == 6) {
				g.css("top", j+i-150)
			}
		};
		d(window).bind("scroll", h);h()
	};
	var e = function(g){
		c = g;
		d(function(){a()})
	};
	return {
		init:e, version:"1.0"
	}
})(jQuery);

Back2top.init();


