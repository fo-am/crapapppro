package foam.starwisp;

import android.app.Activity;
import android.graphics.Color;
import android.graphics.Point;
import android.support.v4.app.FragmentTransaction;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.FrameLayout;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.Projection;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.PolygonOptions;

import java.util.Vector;

public class DrawableMap {

    FrameLayout fram_map;
    Button scribble_button;
    Boolean draw_mode;
    GoogleMap map;

    Vector<Vector<LatLng>> polygons;

    Vector<LatLng> current_polygon;

    public void init(ViewGroup parent, StarwispActivity owner, int ID) {
        draw_mode = false;

        current_polygon = new Vector<LatLng>();
        polygons = new Vector<Vector<LatLng>>();

        FrameLayout outer_map = new FrameLayout(owner);
        outer_map.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
                FrameLayout.LayoutParams.FILL_PARENT));

        FrameLayout map_container = new FrameLayout(owner);
        map_container.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
                400));
        map_container.setId(ID);
        SupportMapFragment mapfrag = SupportMapFragment.newInstance();
        FragmentTransaction fragmentTransaction = owner.getSupportFragmentManager().beginTransaction();
        fragmentTransaction.add(ID,mapfrag);
        fragmentTransaction.commit();
        outer_map.addView(map_container);

        fram_map = new FrameLayout(owner);
        fram_map.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
                FrameLayout.LayoutParams.FILL_PARENT));
        outer_map.addView(fram_map);

        scribble_button  = new Button(owner);
        scribble_button.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.WRAP_CONTENT,
                FrameLayout.LayoutParams.WRAP_CONTENT));
        scribble_button.setTextSize(20);
        scribble_button.setTypeface(((StarwispActivity)owner).m_Typeface);
        scribble_button.setText("New field");

        fram_map.addView(scribble_button);
        parent.addView(outer_map);

        mapfrag.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(GoogleMap googleMap) {
                map = googleMap;
                map.setMapType(GoogleMap.MAP_TYPE_SATELLITE);
                SetupStuff();
            }});

    }

    public void SetupStuff() {

        scribble_button.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                draw_mode = !draw_mode;
                if (!draw_mode) {
                    polygons.add(current_polygon);
                    current_polygon = new Vector<LatLng>();
                    scribble_button.setText("New field");
                } else {
                    scribble_button.setText("Save field");
                }
            }
        });

        fram_map.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                if (!draw_mode) return false;

                float x = event.getX();
                float y = event.getY();

                int x_co = Math.round(x);
                int y_co = Math.round(y);

                Projection projection = map.getProjection();
                Point x_y_points = new Point(x_co, y_co);

                LatLng latLng = map.getProjection().fromScreenLocation(x_y_points);
                double latitude = latLng.latitude;
                double longitude = latLng.longitude;

                int eventaction = event.getAction();
                switch (eventaction) {
                    case MotionEvent.ACTION_DOWN:
                        // finger touches the screen
                        current_polygon.add(new LatLng(latitude, longitude));
                        break;

                    case MotionEvent.ACTION_MOVE:
                        // finger moves on the screen
                        //current_polygon.add(new LatLng(latitude, longitude));
                        break;

                    case MotionEvent.ACTION_UP:
                        // finger leaves the screen
                        Draw_Map();
                        break;
                }

                return true;

            }
        });
    }

    public void Draw_Map() {
        map.clear();

        for (Vector<LatLng> val : polygons) {
            PolygonOptions rectOptions = new PolygonOptions();
            rectOptions.addAll(val);
            rectOptions.strokeColor(0x77ffff55);
            rectOptions.strokeWidth(3);
            rectOptions.fillColor(0x30aaFFaa);
            map.addPolygon(rectOptions);
        }

        PolygonOptions rectOptions = new PolygonOptions();
        rectOptions.addAll(current_polygon);
        rectOptions.strokeColor(0x77ffff55);
        rectOptions.strokeWidth(3);
        rectOptions.fillColor(0x30aaFFaa);
        map.addPolygon(rectOptions);

    }
}


