$(document).ready(function(){	

	//hide the model results initially
	$('#model_out').hide();
	
	//hide the interstitial initially
	$('#building_inter').hide();
	$('#oem_results').hide();
	
	var model_persist = "";
  
$("#show_perf").on("click", function(){
    
	//alert("inside Ensemble Model....");
	
	    $("#show_perf").attr("disabled", "disabled");  
		
		$('#model_opt').hide();
		
		$('#building_inter').show();
	  
	   $("#building_inter").text("Setting up Train & Test...");
	   
	   
	
	//Check which model is selected
	var dvname=$("#dvname").val()
	var preddv=$("#preddv").val()
	
	var isChecked=""
	
	if($('#LR').prop('checked')==true)
		{
		 isChecked="LR"
		} else if($('#RF').prop('checked')==true)
		{
		 isChecked="RF"
		} else if($('#SVM').prop('checked')==true)
		{
		 isChecked="SVM"
		} else if($('#GBM').prop('checked')==true)
		{
		 isChecked="GBM"
		} else if($('#NB').prop('checked')==true)
		{
		 isChecked="NB"
		} else if($('#NNET').prop('checked')==true)
		{
		 isChecked="NNET"
		}
		else if($('#OEM').prop('checked')==true)
		{
		 isChecked="OEM"
		}
		
		$('#building_inter').show();
		
	    $("#building_inter").text("Training the Model... Will be ready in a jiffy!");
		
		
		//alert(isChecked);

		
    //perform the request
    var req = ocpu.call("modelling_module", {
      "DV" : dvname, "model_selection" :  isChecked, "predictorClass" : preddv
    }, function(session){
		$("#building_inter").show().delay(1000).fadeOut(100,showModelResults);
		//get results and display
		$("#building_inter").text("Model Completed! Go check the results now");
    });
    
    //if R returns an error, alert the error message
    req.fail(function(){
      alert("Server error: " + req.responseText);
    });
    
    //after request complete, re-enable the button 
    req.always(function(){
      $("#show_perf").removeAttr("disabled")
    });   

  });

  	function showModelResults()
	{
		var model = $("input[name='radio']:checked").val();
		model_persist = model;
		$('#mod_nm').html('');
		//get the value from radio to display in the table
		$('#mod_nm').append(model);
		$('#model_out').show();
		if(model == "OEM")
		{
			$('#normal_results').hide()
			$('#oem_results').show()
		}
	}
	
	//hide the performance metrics and show the model selection
	$('#show_model_sel').click(function() {
		$('#model_opt').show();
		$('#model_out').hide();
		if(model_persist == "OEM")
		{
			$('#oem_results').hide()
			$('#normal_results').show()
			model_persist = "";
		}
	});
	
	$("#swap_right").click(function() {
		var left_selected =[];
		left_selected = $( "#left_box" ).val();
		console.log(left_selected);
		
		for (each in left_selected){
			var ele = left_selected[each]
			$("#right_box").append(new Option("Var "+ele, ele));
			$("#left_box option[value='"+ele+"']").remove();
		}
	});
	$("#swap_left").click(function() {
		var right_selected =[]
		right_selected = $( "#right_box" ).val();
		console.log(right_selected);
		
		for (each in right_selected){
			var ele1 = right_selected[each]
			$("#left_box").append(new Option("Var "+ele1, ele1));
			$("#right_box option[value='"+ele1+"']").remove();
		}
	});

	$(function () { 
		$('#varSel').multiselect({ 
			buttonText: function(options, select) {
				if (options.length === 0) {
					return 'None selected';
				}
				if (options.length === select[0].length) {
					return 'All selected ('+select[0].length+')';
				}
				else if (options.length >= 4) {
					return options.length + ' selected';
				}
				else {
					var labels = [];
					console.log(options);
					options.each(function() {
						labels.push($(this).val());
					});
					return labels.join(', ') + '';
				}
			}
		
		});
	});
  

  });

