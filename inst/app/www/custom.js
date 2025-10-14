// Shiny.addCustomMessageHandler('message',
// 	function(msg) {
// 		alert(JSON.stringify(msg));
// 	}
// );
Shiny.addCustomMessageHandler('message',
	function(message) {
		var H = '#9F9F9F'
		var R = 159
		var G = 159
		var B = 159
		if(message.status === 0){
			H = '#B13C2E'
			R = 177
			G = 60
			B = 46
		}
		var box = bootbox.dialog({
			title: message.header,
			message: message.message
		});
		box.find('.modal-content').css({ border: '4px solid rgba(' + R + ',' + G + ',' + B + ', .7)', 'border-radius': '5px' }) 
		box.find('.modal-header').css({ 'background-color': H, color: '#FFFFFF', 'font-weight': 'bold' }) 
	}
);

