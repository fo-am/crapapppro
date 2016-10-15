package foam.starwisp;

import android.app.Activity;
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
import android.widget.TextView;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.Projection;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PolygonOptions;

import org.json.JSONArray;
import org.json.JSONException;

import java.util.Vector;

public class DrawableMap {
    FrameLayout fram_map;
    Button scribble_button;
    Boolean draw_mode;
    GoogleMap map;
    String map_mode;
    String selected_polygon;
    int ID;

    StarwispActivity m_Context;
    StarwispBuilder m_Builder;

    Vector<LatLng> current_polygon;

    class Polygon {
        String m_UniqueID;
        String m_Name;
        Vector<LatLng> m_Verts;
    }

    Vector<Polygon> polygons;

    public void init(int id, ViewGroup parent, StarwispActivity c, StarwispBuilder b, String mode) {
        draw_mode = false;
        m_Context=c;
        m_Builder=b;
        map_mode = mode;
        ID = id;
        current_polygon = new Vector<LatLng>();
        polygons = new Vector<Polygon>();

        FrameLayout outer_map = new FrameLayout(c);
        outer_map.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
                FrameLayout.LayoutParams.FILL_PARENT));

        FrameLayout map_container = new FrameLayout(c);
        map_container.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
                400));

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
            scribble_button = new Button(c);
            scribble_button.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.WRAP_CONTENT,
                    FrameLayout.LayoutParams.WRAP_CONTENT));
            scribble_button.setTextSize(20);
            scribble_button.setTypeface(((StarwispActivity) c).m_Typeface);
            scribble_button.setText("New field");
            fram_map.addView(scribble_button);
        } else {
            draw_mode=true;
        }

        parent.addView(outer_map);

        mapfrag.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(GoogleMap googleMap) {
                map = googleMap;
                map.setMapType(GoogleMap.MAP_TYPE_SATELLITE);
                SetupStuff();
                DrawMap();
            }});

    }

    public void Clear() {
        current_polygon.clear();
        polygons.clear();
    }

    public void UpdateFromJSON(JSONArray map) {
        Clear();
        // json format
        // [ current_polygon [ polygon, polygon, ... ]]
        // polygon:
        // [ name [ latlng, latlng, ...]]
        // latlng:

        // (map may not exist yet when called from update)
        try {
            selected_polygon = map.getString(0);
            JSONArray polygon_list = map.getJSONArray(1);
            for (int i=0; i<polygon_list.length(); i++) {
                JSONArray poly = polygon_list.getJSONArray(i);
                Polygon new_poly = new Polygon();
                new_poly.m_Name = poly.getString(0);
                new_poly.m_UniqueID = poly.getString(1);
                JSONArray verts = poly.getJSONArray(2);
                new_poly.m_Verts = new Vector<LatLng>();
                for (int v=0; v<verts.length(); v++) {
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
                        poly.m_Name = "no name";
                        polygons.add(poly);
                        SendPolygon(poly.m_Verts);
                        current_polygon = new Vector<LatLng>();
                        scribble_button.setText("Draw field");
                    } else {
                        scribble_button.setText("Save field");
                    }
                }
            });
        }

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
                        if (map_mode.equals("edit")) {
                            current_polygon.add(new LatLng(latitude, longitude));
                        } else {
                            String clicked_in = CheckPolygons(latitude, longitude);
                            Log.i("starwisp","clicked in retunred "+clicked_in);
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
            AddText(GetCentre(poly), poly.m_Name, 10, 20);
        }

        if (current_polygon.size()!=0) {
            PolygonOptions rectOptions = new PolygonOptions();
            rectOptions.addAll(current_polygon);
            rectOptions.strokeColor(0x77ffff55);
            rectOptions.strokeWidth(3);
            rectOptions.fillColor(0x30aaFFaa);
            map.addPolygon(rectOptions);
        }
    }

    public Marker AddText(final LatLng location, final String text, final int padding, final int fontSize) {
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


