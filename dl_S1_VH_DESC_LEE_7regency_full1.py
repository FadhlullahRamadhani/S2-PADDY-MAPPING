# Import the Earth Engine Python Package
import ee
import time
import os
import math
from datetime import datetime

# Initialize the Earth Engine object, using the authentication credentials.
ee.Initialize()
def getS1_DESC(img):
	VH = img.select(['VH'])	
	VV = img.select(['VV'])
	angle = img.select(['angle'])
	return img.select([]).addBands([VH,VV,angle]).select([0,1,2],['VH','VV','angle'])  


#Function to convert from dB
def toNatural(img):
	return ee.Image(10.0).pow(img.select(0).divide(10.0));


#Function to convert to dB
def toDB(img):
	return ee.Image(img).log10().multiply(10.0);

#Apllying a Refined Lee Speckle filter as coded in the SNAP 3.0 S1TBX:
#https:#github.com/senbox-org/s1tbx/blob/master/s1tbx-op-sar-processing/src/main/java/org/esa/s1tbx/sar/gpf/filtering/SpeckleFilters/RefinedLee.java
def RefinedLee(img):
  # img must be in natural units, i.e. not in dB!
  # Set up 3x3 kernels
   
  # convert to natural.. do not apply function on dB!
  myimg = toNatural(img);
   
  weights3 = ee.List.repeat(ee.List.repeat(1,3),3);
  kernel3 = ee.Kernel.fixed(3,3, weights3, 1, 1, False);
   
  mean3 = myimg.reduceNeighborhood(ee.Reducer.mean(), kernel3);
  variance3 = myimg.reduceNeighborhood(ee.Reducer.variance(), kernel3);
   
  # Use a sample of the 3x3 windows inside a 7x7 windows to determine gradients and directions

  sample_weights = ee.List([[0,0,0,0,0,0,0], [0,1,0,1,0,1,0],[0,0,0,0,0,0,0], [0,1,0,1,0,1,0], [0,0,0,0,0,0,0], [0,1,0,1,0,1,0],[0,0,0,0,0,0,0]]);
 
  sample_kernel = ee.Kernel.fixed(7,7, sample_weights, 3,3, False);
   
  # Calculate mean and variance for the sampled windows and store as 9 bands
  sample_mean = mean3.neighborhoodToBands(sample_kernel);
  sample_var= variance3.neighborhoodToBands(sample_kernel);
   
  # Determine the 4 gradients for the sampled windows
  gradients = sample_mean.select(1).subtract(sample_mean.select(7)).abs();
  gradients = gradients.addBands(sample_mean.select(6).subtract(sample_mean.select(2)).abs());
  gradients = gradients.addBands(sample_mean.select(3).subtract(sample_mean.select(5)).abs());
  gradients = gradients.addBands(sample_mean.select(0).subtract(sample_mean.select(8)).abs());
   
  # And find the maximum gradient amongst gradient bands
  max_gradient = gradients.reduce(ee.Reducer.max());
   
  # Create a mask for band pixels that are the maximum gradient
  gradmask = gradients.eq(max_gradient);
   
  # duplicate gradmask bands: each gradient represents 2 directions
  gradmask = gradmask.addBands(gradmask);
   
  # Determine the 8 directions
  directions = sample_mean.select(1).subtract(sample_mean.select(4)).gt(sample_mean.select(4).subtract(sample_mean.select(7))).multiply(1);
  directions = directions.addBands(sample_mean.select(6).subtract(sample_mean.select(4)).gt(sample_mean.select(4).subtract(sample_mean.select(2))).multiply(2));
  directions = directions.addBands(sample_mean.select(3).subtract(sample_mean.select(4)).gt(sample_mean.select(4).subtract(sample_mean.select(5))).multiply(3));
  directions = directions.addBands(sample_mean.select(0).subtract(sample_mean.select(4)).gt(sample_mean.select(4).subtract(sample_mean.select(8))).multiply(4));
  # The next 4 are the not() of the previous 4
  directions = directions.addBands(directions.select(0).Not().multiply(5));
  directions = directions.addBands(directions.select(1).Not().multiply(6));
  directions = directions.addBands(directions.select(2).Not().multiply(7));
  directions = directions.addBands(directions.select(3).Not().multiply(8));
   
  # Mask all values that are not 1-8
  directions = directions.updateMask(gradmask);
   
  # "collapse" the stack into a singe band image (due to masking, each pixel has just one value (1-8) in it's directional band, and is otherwise masked)
  directions = directions.reduce(ee.Reducer.sum());
   
  sample_stats = sample_var.divide(sample_mean.multiply(sample_mean));
   
  # Calculate localNoiseVariance
  sigmaV = sample_stats.toArray().arraySort().arraySlice(0,0,5).arrayReduce(ee.Reducer.mean(), [0]);
   
  # Set up the 7*7 kernels for directional statistics
  rect_weights = ee.List.repeat(ee.List.repeat(0,7),3).cat(ee.List.repeat(ee.List.repeat(1,7),4));

  diag_weights = ee.List([[1,0,0,0,0,0,0], [1,1,0,0,0,0,0], [1,1,1,0,0,0,0],
  [1,1,1,1,0,0,0], [1,1,1,1,1,0,0], [1,1,1,1,1,1,0], [1,1,1,1,1,1,1]]);
   
  rect_kernel = ee.Kernel.fixed(7,7, rect_weights, 3, 3, False);
  diag_kernel = ee.Kernel.fixed(7,7, diag_weights, 3, 3, False);
   
  # Create stacks for mean and variance using the original kernels. Mask with relevant direction.
  dir_mean = myimg.reduceNeighborhood(ee.Reducer.mean(), rect_kernel).updateMask(directions.eq(1));
  dir_var = myimg.reduceNeighborhood(ee.Reducer.variance(), rect_kernel).updateMask(directions.eq(1));
   
  dir_mean = dir_mean.addBands(myimg.reduceNeighborhood(ee.Reducer.mean(), diag_kernel).updateMask(directions.eq(2)));
  dir_var= dir_var.addBands(myimg.reduceNeighborhood(ee.Reducer.variance(), diag_kernel).updateMask(directions.eq(2)));
 
  # and add the bands for rotated kernels
  for i in range(1, 4):
	  dir_mean = dir_mean.addBands(myimg.reduceNeighborhood(ee.Reducer.mean(), rect_kernel.rotate(i)).updateMask(directions.eq(2*i+1)));
	  dir_var = dir_var.addBands(myimg.reduceNeighborhood(ee.Reducer.variance(), rect_kernel.rotate(i)).updateMask(directions.eq(2*i+1)));
	  dir_mean = dir_mean.addBands(myimg.reduceNeighborhood(ee.Reducer.mean(), diag_kernel.rotate(i)).updateMask(directions.eq(2*i+2)));
	  dir_var = dir_var.addBands(myimg.reduceNeighborhood(ee.Reducer.variance(), diag_kernel.rotate(i)).updateMask(directions.eq(2*i+2)));
	  
   
  # "collapse" the stack into a single band image (due to masking, each pixel has just one value in it's directional band, and is otherwise masked)
  dir_mean = dir_mean.reduce(ee.Reducer.sum());
  dir_var = dir_var.reduce(ee.Reducer.sum());
   
  # A finally generate the filtered value
  varX = dir_var.subtract(dir_mean.multiply(dir_mean).multiply(sigmaV)).divide(sigmaV.add(1.0));
   
  b = varX.divide(dir_var);
   
  result = dir_mean.add(b.multiply(myimg.subtract(dir_mean)));
  #return(result);

  return(img.select([]).addBands(ee.Image(toDB(result.arrayGet(0)))));


	
regency_code=range(7)
regency_code[0] = '3212'
regency_code[1] = '3213'
regency_code[2] = '3215'
regency_code[3] = '3517'
regency_code[4] = '3518'
regency_code[5] = '3522'
regency_code[6] = '3524'




regency_region=range(7)
regency_region[0] = [[[107.85008931002845,-6.6767776647842068],[108.53938569317057,-6.6807052436117607],[108.54155646158407,-6.2311203918922091],[107.85286599975034,-6.2274592071470662],[107.85008931002845,-6.6767776647842068]]]
regency_region[1] = [[[107.52337571659255,-6.8128015829719715],[107.92213880176756,-6.8155095834678603],[107.92598795047394,-6.1825661762315001],[107.5277212504932,-6.1801116433829586],[107.52337571659255,-6.8128015829719715]]]
regency_region[2] = [[[107.07988956805879,-6.5897470400645339],[107.641099075348,-6.5938449787380691],[107.64528418122191,-5.9397937627877129],[107.08476987986732,-5.9361052670271839],[107.07988956805879,-6.5897470400645339]]]
regency_region[3] = [[[112.06352250278002,-7.7803827245697512],[112.4558336701248,-7.7792180647653097],[112.45437365965267,-7.343355576713174],[112.0624557152223,-7.3444542691365049],[112.06352250278002,-7.7803827245697512]]]
regency_region[4] = [[[111.72553754845603,-7.8367006139159177],[112.17005707933524,-7.8357007452640008],[112.1688579934099,-7.3934166933220657],[111.72479388343491,-7.3943594997541435],[111.72553754845603,-7.8367006139159177]]]
regency_region[5] = [[[111.42374487956054,-7.4701085826404405],[112.16668911962644,-7.4687705849073245],[112.16544869622754,-6.9850046460977149],[111.42329424622658,-6.9862551195647171],[111.42374487956054,-7.4701085826404405]]]
regency_region[6] = [[[112.0726867405661,-7.3849218597130912],[112.55351188021343,-7.3835077819192065],[112.55175861673327,-6.8624613283077442],[112.07147581705456,-6.8637746577719465],[112.0726867405661,-7.3849218597130912]]]



#12_S1_SR_05Jul-21Jul-2017.tif


for i in range(6, 7):
	regency_poly = ee.Geometry.Polygon(regency_region[i])
	regency_code_str = regency_code[i]
	S1_Collection = ee.ImageCollection('COPERNICUS/S1_GRD').filterDate('2018-05-01','2018-12-31')
	#S1_Collection = ee.ImageCollection('COPERNICUS/S1_GRD').filterDate('2013-05-01','2020-05-15')
	#S1_Collection = ee.ImageCollection('COPERNICUS/S1_GRD').filterDate('2018-06-01','2018-08-01')
	S1_Collection = S1_Collection.filterBounds(regency_poly)
	S1_Collection = S1_Collection.sort('DATE_ACQUIRED')
	S1_Collection = S1_Collection.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
	S1_Collection = S1_Collection.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
	S1_Collection = S1_Collection.filter(ee.Filter.eq('instrumentMode', 'IW'))
	S1_Collection = S1_Collection.filter(ee.Filter.eq('orbitProperties_pass', 'DESCENDING'))
	S1_Collection = S1_Collection.filter(ee.Filter.eq('resolution_meters', 10))
	S1_Collection = S1_Collection.select(['VH'])
	S1_Collection = S1_Collection.map(RefinedLee)
 
	colList = S1_Collection.toList(1000)
	n = colList.size().getInfo();
	print  regency_code_str
	print n
	for j in range(0, n):
		img = ee.Image(colList.get(j));		
		id = img.id().getInfo();
		filename = 'S1-DESC-VH-LEE-10m-utm-'+ regency_code_str + '-' + id
		fullname_check = "F:\\RS_images\\S1-DESC-VH-LEE-10m-utm\\" + filename +  '.tif'
		if os.path.isfile(fullname_check):
			print datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' sudah didownload->  '  + str(j+1) + ' - ' + filename
		else:		
			task_config = {
			  'description': filename,
			  'scale': 10,
			  'crs' : 'EPSG:32749',
			  'region': regency_region[i],
			  'driveFolder': 'S1-DESC-VH-LEE-10m-utm'
			}
			task = ee.batch.Export.image(img, filename, task_config)
			task.start()
			print datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' ->  '  + str(j+1) + ' - ' + filename


