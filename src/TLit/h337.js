(function(){
	var config = {
		container: document.getElementById('h337'),
		radius: 10,
		maxOpacity: .5,
		minOpacity: 0,
		blur: .75
	};
	// create heatmap with configuration
	var heatmapInstance = h337.create(config);
	var data = {
		max: 10000,
		min: 0,
		data: []
	};
	
	function data2blob(data,isBase64)
	{
		var chars="";
		if (isBase64) chars=atob(data); else chars=data;
		var bytes=new Array(chars.length);
		for (var i=0;i<chars.length; i++) bytes[i]=chars.charCodeAt(i);
		var blob=new Blob([new Uint8Array(bytes)]);
		return blob;
	}
	
	heatmapInstance.setData(data);

        $("body").mousemove(function(e) {
    		var posX = e.pageX;
    		var posY = e.pageY;
    		var val = heatmapInstance.getValueAt({ x: posX, y: posY });
    		heatmapInstance.addData({ x: posX, y: posY, value: val+1});
		});
		
	$(window).on("beforeunload", function() { 
		 console.log(heatmapInstance.getData());
		 var myString = JSON.stringify(heatmapInstance.getData());
		 var d = new Date();
		 var n = d.getTime();
		 saveAs( data2blob(myString), n+"heatmap.json" );
	});
	
	$(window).unload( function() {
		var myString = JSON.stringify(heatmapInstance.getData());
		var d = new Date();
		var n = d.getTime();
		saveAs( data2blob(myString), n+"heatmap.json" );
	});
})()