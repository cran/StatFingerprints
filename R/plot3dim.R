#########################################################
#    3D function to plot all profiles of the project    #
#########################################################

"plot3dim"<-function(mat,col)
{
  require(rgl) || stop("rgl library not available")
  open3d()
  bg3d("slategray")
  x<-1:dim(mat)[2]
  y<-1:dim(mat)[1]
  z=t(mat);  
  rgl.viewpoint(zoom=0.4,theta = 0, phi = -55, fov = 60)
  persp3d(x= x,y=y, z=z,aspect=c(1,0.5,0.5), main="Rotation: left click. Zoom: rigth click",axes = FALSE, box = FALSE, col=myp,xlab = "Communities", ylab = "scan of the chromatogram", zlab = "relative abundance")
}
