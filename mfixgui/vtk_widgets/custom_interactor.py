'''
Custom interactor that allows for picking of vtk objects.
'''
import vtk


class CustomInteractorStyle(vtk.vtkInteractorStyleTrackballCamera):
    """custom vtkInteractorStyleTrackballCamera to highlight selected
    objects"""
    def __init__(self, parent=None):
        self.AddObserver("LeftButtonPressEvent", self.left_button_press_event)

        self.last_picked_actor = None
        self.last_picked_property = vtk.vtkProperty()

    def left_button_press_event(self, obj, event):
        """on a left mouse press event, see if there is an actor and highlight
        it"""
        clickPos = self.GetInteractor().GetEventPosition()

        picker = vtk.vtkPropPicker()
        picker.Pick(clickPos[0], clickPos[1], 0, self.GetDefaultRenderer())

        # get the new actor
        self.new_picked_actor = picker.GetActor()

        # If something was selected
        if self.new_picked_actor:
            # If we picked something before, reset its property
            if self.last_picked_actor:
                self.last_picked_actor.GetProperty().DeepCopy(
                    self.last_picked_property)

            # Save the property of the picked actor so that we can
            # restore it next timefrom mfixgui.widgets.
            self.last_picked_property.DeepCopy(self.new_picked_actor.GetProperty())
            # Highlight the picked actor by changing its properties
            self.new_picked_actor.GetProperty().SetColor(255/255.0, 140/255.0, 0)
            self.new_picked_actor.GetProperty().SetDiffuse(1.0)
            self.new_picked_actor.GetProperty().SetSpecular(0.0)

            # save the last picked actor
            self.last_picked_actor = self.new_picked_actor

        # clear selection
        elif self.last_picked_actor:
            self.last_picked_actor.GetProperty().DeepCopy(
                self.last_picked_property)
            self.last_picked_actor = None

        self.OnLeftButtonDown()
        return
