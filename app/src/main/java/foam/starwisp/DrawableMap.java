// Starwisp Copyright (C) 2016 Dave Griffiths
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package foam.starwisp;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.Rect;
import android.support.v4.app.FragmentTransaction;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.text.Html;
import android.view.Gravity;

import com.google.android.gms.maps.CameraUpdate;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.Projection;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.CameraPosition;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PolygonOptions;
import com.google.android.gms.maps.model.CircleOptions;
import com.google.android.gms.maps.model.PolylineOptions;

import org.json.JSONArray;
import org.json.JSONException;

import java.util.Vector;

public class DrawableMap {
    FrameLayout fram_map;
    LinearLayout map_cont;
    Button scribble_button;
    Boolean draw_mode;
    Boolean button_mode;
    GoogleMap map;
    String map_mode;
    String selected_polygon;
    String selected_polygon_name;
    int ID;

    boolean map_ready;
    double centre_lat;
    double centre_lon;
    int centre_zoom;

    boolean draw_indicator;
    double indicator_lat;
    double indicator_lon;

    StarwispActivity m_Context;
    StarwispBuilder m_Builder;
    
    TextView m_instructions;

    Vector<LatLng> current_polygon;

    class Polygon {
        String m_UniqueID;
        String m_Name;
        Vector<String> m_Info;
        Vector<LatLng> m_Verts;
    }

    ViewGroup m_parent;

    Vector<Polygon> polygons;

    public void init(int id, ViewGroup parent, StarwispActivity c, StarwispBuilder b, String mode) {
	m_parent=parent;
        map_ready = false;
        draw_mode = false;
        button_mode = false;
        m_Context=c;
        m_Builder=b;
        map_mode = mode;
        ID = id;
        current_polygon = new Vector<LatLng>();
        polygons = new Vector<Polygon>();
        centre_lat=49.198935;
        centre_lon=2.988281;
        centre_zoom=4;
	draw_indicator=false;
	indicator_lat=0;
	indicator_lon=0;

        FrameLayout outer_map = new FrameLayout(c);
        outer_map.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
                FrameLayout.LayoutParams.FILL_PARENT));

        FrameLayout map_container = new FrameLayout(c);
        map_container.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
                FrameLayout.LayoutParams.FILL_PARENT));

        map_container.setId(ID);
        SupportMapFragment mapfrag = SupportMapFragment.newInstance();
        FragmentTransaction fragmentTransaction = c.getSupportFragmentManager().beginTransaction();
        fragmentTransaction.add(ID,mapfrag);
        fragmentTransaction.commit();
        outer_map.addView(map_container);

        fram_map = new FrameLayout(c);
        fram_map.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
							      FrameLayout.LayoutParams.FILL_PARENT));
        outer_map.addView(fram_map);
	

        if (map_mode.equals("edit")) {
	    map_cont = new LinearLayout(c);
	    map_cont.setOrientation(LinearLayout.VERTICAL);
	    LinearLayout.LayoutParams lp = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.FILL_PARENT,								     									
									 LinearLayout.LayoutParams.FILL_PARENT);
	    lp.gravity=Gravity.CENTER;
	    map_cont.setLayoutParams(lp);
	    
            scribble_button = new Button(c);
	    lp = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT,
					       LinearLayout.LayoutParams.WRAP_CONTENT);
	    lp.gravity=Gravity.CENTER;
            scribble_button.setLayoutParams(lp);

            scribble_button.setTextSize(20);
            scribble_button.setTypeface(((StarwispActivity) c).m_Typeface);
            scribble_button.setText("Draw field");
            map_cont.addView(scribble_button);
	    
	    m_instructions = new TextView(c);
            m_instructions.setLayoutParams(lp);
	    m_instructions.setTextSize(20);
	    m_instructions.setTypeface(m_Context.m_Typeface);
	    m_instructions.setTextColor(Color.WHITE);
	    // arg i18n
            map_cont.addView(m_instructions);

	    fram_map.addView(map_cont);
	    
        } else {
            //button_mode=true;
        }


        parent.addView(outer_map);

        mapfrag.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(GoogleMap googleMap) {
                map = googleMap;
                map.setMapType(GoogleMap.MAP_TYPE_SATELLITE);
        		map.setMyLocationEnabled(true);
	        	map.getUiSettings().setZoomControlsEnabled(true);
                SetupStuff();
                DrawMap();
                Log.i("starwisp","map made");
                Log.i("starwisp","updating map centre to "+centre_lat+" "+centre_lon);
                //CameraUpdate center_map=CameraUpdateFactory.newLatLng(new LatLng(centre_lat,centre_lon));
                //CameraUpdate zoom_map=CameraUpdateFactory.zoomTo(centre_zoom);
                //map.moveCamera(center_map);
                //map.animateCamera(zoom_map);

                CameraPosition cameraPosition = new CameraPosition.Builder()
                    .target(new LatLng(centre_lat, centre_lon)).zoom(centre_zoom).build();
                 map.animateCamera(CameraUpdateFactory
                    .newCameraPosition(cameraPosition));

                map_ready=true;
            }});

    }

    public void Clear() {
        current_polygon.clear();
        polygons.clear();
    }

    public void Centre(double lat, double lng, int z) {
        centre_lat = lat;
        centre_lon = lng;
        centre_zoom = z;

        Log.i("starwisp","updating map centre to "+lat+" "+lng);

        if (map_ready) {
            CameraUpdate center_map=CameraUpdateFactory.newLatLng(new LatLng(centre_lat,centre_lon));
            CameraUpdate zoom_map=CameraUpdateFactory.zoomTo(centre_zoom);
            map.moveCamera(center_map);
            map.animateCamera(zoom_map);
        }
    }

    public void RemoveSelected() {
	Vector<Polygon> new_polygons = new Vector<Polygon>();
        for (Polygon poly : polygons) {
	    if (!poly.m_UniqueID.equals(selected_polygon)) {
		new_polygons.add(poly);
	    }
	}
	polygons = new_polygons;
    }

    public void UpdateFromJSON(JSONArray map) {
        Clear();
        // json format
        // [ current_polygon(id) [ polygon, polygon, ... ]]
        // polygon:
        // [ name uid infotext [ latlng, latlng, ...]]
        // latlng:

        // (map may not exist yet when called from update)
        try {
            selected_polygon = map.getString(0);
            JSONArray polygon_list = map.getJSONArray(1);
            for (int i=0; i<polygon_list.length(); i++) {

		Log.e("starwisp", "poly "+i);

                JSONArray poly = polygon_list.getJSONArray(i);
                Polygon new_poly = new Polygon();
                new_poly.m_Name = poly.getString(0);
                new_poly.m_UniqueID = poly.getString(1);
		JSONArray info_list= poly.getJSONArray(2);
		new_poly.m_Info = new Vector<String>();
		for (int j=0; j<info_list.length(); j++) {
		    new_poly.m_Info.add(info_list.getString(j));
		}

		// pick out the selected poly's name
		if (new_poly.m_UniqueID.equals(selected_polygon)) {
		    selected_polygon_name=new_poly.m_Name;
		}

                JSONArray verts = poly.getJSONArray(3);
                new_poly.m_Verts = new Vector<LatLng>();
                for (int v=0; v<verts.length(); v++) {
		    Log.e("starwisp", "vert "+v);
                    JSONArray latlng = verts.getJSONArray(v);
                    new_poly.m_Verts.add(new LatLng(latlng.getDouble(0),latlng.getDouble(1)));
                }
                if (new_poly.m_Verts.size()>0) {
                    polygons.add(new_poly);
                }
            }
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing data in drawable map " + e.toString());
        }

	if (map_ready) {
	    Log.e("starwisp", "Redrawing map after JSON update");
 	    DrawMap();
	}
    }



    public void SendPolygon(Vector<LatLng> polygon) {
        String str="(";
        for (LatLng latlng : polygon) {
            str+="("+Double.toString(latlng.latitude)+" "+Double.toString(latlng.longitude)+") ";
        }
        str+=")";
        m_Builder.CallbackArgs(m_Context,m_Context.m_Name,ID,str);
    }

    public void SetupStuff() {

        if (map_mode.equals("edit")) {
            scribble_button.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    draw_mode = !draw_mode;
                    if (!draw_mode) {
                        Polygon poly = new Polygon();
                        poly.m_Verts = current_polygon;
                        poly.m_Name = selected_polygon_name;
                        poly.m_UniqueID = selected_polygon;
                        polygons.add(poly);
                        SendPolygon(poly.m_Verts);
                        current_polygon = new Vector<LatLng>();
                        scribble_button.setText("Draw field");
			m_instructions.setText("");
			draw_indicator=false;
			DrawMap();
                    } else {
			RemoveSelected();
			DrawMap();
                        scribble_button.setText("Save field");
			m_instructions.setText("Touch each corner of your field to draw around it.");
                    }
                }
            });
        }

        fram_map.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                if (!draw_mode && !button_mode) return false;

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
                        if (map_mode.equals("edit")) {
			    draw_indicator=true;
			    indicator_lat = latitude;
			    indicator_lon = longitude;
                            current_polygon.add(new LatLng(latitude, longitude));
                        } else {
                            String clicked_in = CheckPolygons(latitude, longitude);
                            if (!clicked_in.equals("")) {
                                m_Builder.CallbackArgs(m_Context,m_Context.m_Name,ID,"\""+clicked_in+"\"");
                            }
                        }
                        break;

                    case MotionEvent.ACTION_MOVE:
                        // finger moves on the screen
                        break;

                    case MotionEvent.ACTION_UP:
                        // finger leaves the screen
                        DrawMap();
                        break;
                }

                if (button_mode) return false;

                return true;

            }
        });
    }

    LatLng GetCentre(Polygon poly) {
        Double centrex = 0.0;
        Double centrey = 0.0;
        for (LatLng latlng : poly.m_Verts) {
            centrex+=latlng.latitude;
            centrey+=latlng.longitude;
        }
        centrex/=poly.m_Verts.size();
        centrey/=poly.m_Verts.size();
        return new LatLng(centrex,centrey);
     }

    Boolean IsInPolygon(Polygon poly, double x, double y) {
        int intersectionCount = 0;
        double x0 = poly.m_Verts.lastElement().latitude - x;
        double y0 = poly.m_Verts.lastElement().longitude - y;
        for (LatLng vert : poly.m_Verts) {
            double x1 = vert.latitude - x;
            double y1 = vert.longitude - y;
            if (y0 > 0 && y1 <= 0 && x1 * y0 > y1 * x0) {
                intersectionCount++;
            }
            if (y1 > 0 && y0 <= 0 && x0 * y1 > y0 * x1) {
                intersectionCount++;
            }
            x0 = x1;
            y0 = y1;
        }
        return (intersectionCount % 2) == 1;
    }

    String CheckPolygons(double x, double y) {
        for (Polygon poly : polygons) {
            if (IsInPolygon(poly,x,y)) {
                return poly.m_UniqueID;
            }
        }
        return "";
    }

    public void DrawMap() {
        map.clear();

        for (Polygon poly : polygons) {
            PolygonOptions rectOptions = new PolygonOptions();
            rectOptions.addAll(poly.m_Verts);
            if (selected_polygon.equals(poly.m_UniqueID)) {
                rectOptions.strokeColor(0x77ffff55);
            } else {
                rectOptions.strokeColor(0x77aaFFaa);
            }
            rectOptions.strokeWidth(3);
            rectOptions.fillColor(0x30aaFFaa);
            map.addPolygon(rectOptions);

	    // only show text for one field in edit mode
	    if (!map_mode.equals("edit")) {
		AddText(GetCentre(poly), poly.m_Name, 40, 20, Color.WHITE);
		for (int j=0; j<poly.m_Info.size(); j++) {
		    AddText(GetCentre(poly), poly.m_Info.get(j), j*20, 14, 0xffccFFcc);
		}
	    } else {
		if (selected_polygon.equals(poly.m_UniqueID)) {
		    AddText(GetCentre(poly), poly.m_Name, 0, 20, Color.WHITE);
		}
	    }

        }

        if (current_polygon.size()!=0) {
            PolygonOptions rectOptions = new PolygonOptions();
            rectOptions.addAll(current_polygon);
            rectOptions.strokeColor(0x77ffff55);
            rectOptions.strokeWidth(3);
            rectOptions.fillColor(0x30aaFFaa);
            map.addPolygon(rectOptions);
        }

	if (draw_indicator) {
	    CircleOptions iOptions = new CircleOptions();
            iOptions.center(new LatLng(indicator_lat,indicator_lon));
            iOptions.radius(50);
            iOptions.strokeColor(0xffffffff);
            iOptions.strokeWidth(1);
            iOptions.fillColor(0x00000000);
            map.addCircle(iOptions);
	    {
            PolylineOptions pOptions = new PolylineOptions();
            pOptions.add(new LatLng(indicator_lat+0.0007,indicator_lon));
            pOptions.add(new LatLng(indicator_lat-0.0007,indicator_lon));
            pOptions.color(0xffffffff);
            pOptions.width(1);
            map.addPolyline(pOptions);
	    }
	    {
            PolylineOptions pOptions = new PolylineOptions();
            pOptions.add(new LatLng(indicator_lat,indicator_lon+0.001));
            pOptions.add(new LatLng(indicator_lat,indicator_lon-0.001));
            pOptions.color(0xffffffff);
            pOptions.width(1);
            map.addPolyline(pOptions);
	    }
	}
    }

    public Marker AddText(final LatLng location, final String text, final int padding, final int fontSize, int colour) {
        Marker marker = null;

        final TextView textView = new TextView(m_Context);
        textView.setText(text);
        textView.setTextSize(fontSize);
        textView.setTypeface(m_Context.m_Typeface);

        final Paint paintText = textView.getPaint();

        final Rect boundsText = new Rect();
        paintText.getTextBounds(text, 0, textView.length(), boundsText);
        paintText.setTextAlign(Paint.Align.CENTER);

        final Bitmap.Config conf = Bitmap.Config.ARGB_8888;
        final Bitmap bmpText = Bitmap.createBitmap(boundsText.width() + 2
              * padding, boundsText.height() + 2 * padding, conf);

        final Canvas canvasText = new Canvas(bmpText);
        paintText.setColor(Color.BLACK);
	
        canvasText.drawText(text, (canvasText.getWidth() / 2)+3,
			    (canvasText.getHeight() - padding - boundsText.bottom)+3, paintText);
	
        paintText.setColor(colour);
	
        canvasText.drawText(text, canvasText.getWidth() / 2,
			    canvasText.getHeight() - padding - boundsText.bottom, paintText);
	
        final MarkerOptions markerOptions = new MarkerOptions()
                .position(location)
                .icon(BitmapDescriptorFactory.fromBitmap(bmpText))
                .anchor(0.5f, 1);

        marker = map.addMarker(markerOptions);

        return marker;
    }
}


