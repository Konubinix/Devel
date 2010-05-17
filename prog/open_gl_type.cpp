#include <GL/glut.h>

float angle = 0;

void renderScene(void) {

	// notice that we're now clearing the depth buffer 
	// as well this is required, otherwise the depth buffer 
	// gets filled and nothing gets rendered. 
	// Try it out, remove the depth buffer part.
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	// save the previous settings, in this case save 
	// we're refering to the camera settings.
	glPushMatrix();
	
	// Perform a rotation around the y axis (0,1,0) 
	// by the amount of degrees defined in the variable angle
	glRotatef(angle,0.0,1.0,0.0);
	glBegin(GL_TRIANGLES);
		glVertex3f(-0.5,-0.5,0.0);
		glVertex3f(0.5,0.0,0.0);
		glVertex3f(0.0,0.5,0.0);
	glEnd();
	
	// discard the modelling transformations
	// after this the matrix will have only the camera settings.
	glPopMatrix();
	
	// swapping the buffers causes the rendering above to be 
	// shown
	glutSwapBuffers();
	
	// finally increase the angle for the next frame
	angle++;
}

int main(int argc, char **argv) {
	glutInit(&argc, argv);
	
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	
	glutInitWindowPosition(100,100);
	glutInitWindowSize(320,320);
	glutCreateWindow("Window");
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	// enable depth testing
	glEnable(GL_DEPTH_TEST);
	glutMainLoop();
	return 0;
}
